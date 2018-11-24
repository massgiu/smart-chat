import Client._
import GroupChatServer.{DoesContainsMembersInList, RemoveMember}
import OneToOneChatServer.DoesContainsMembers
import RegisterServer._
import Utils.ifNewNameIsValidOrElse
import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class RegisterServer extends Actor with Stash {

  //futures' callbacks will be executed on the messageDispatcher, which is an executionContext
  import context.dispatcher
  implicit val timeout: Timeout = Timeout(5 seconds)
  val model = new RegisterModel

  override def preStart(): Unit = {
    println("RegisterServer started...waiting for new client")
  }

  override def receive: Receive = {
    case JoinRequest(clientName) => model.addNewUser(clientName, sender)
      .ifSuccess(_ => {sender ! AcceptRegistrationFromRegister(true); sendNewServersToAllClients()})
      .orElse(_ => sender ! AcceptRegistrationFromRegister(false))
    case Unjoin() => unregisterUser(sender); sendNewServersToAllClients()
    case NewOneToOneChatRequest(friendName) => createNewOneToOneChat(friendName)
      .ifSuccess(_ => sender ! ResponseForChatCreation(accept = true))
      .orElse(_ => sender ! ResponseForChatCreation(accept = false))
    case NewGroupChatRequest(newGroupName) => createNewGroupChat(newGroupName)
      .ifSuccess(_ => {sender ! ResponseForChatCreation(true); sendNewServersToAllClients()})
      .orElse(_ => sender ! ResponseForChatCreation(false))
    case AllUsersAndGroupsRequest => model.getAllUsersAndGroupsNames
      .ifSuccess(usersAndGroups => sender ! UserAndGroupActive(usersAndGroups.head._1, usersAndGroups.head._2))
      .orElse(_ => sender ! UserAndGroupActive(List.empty, List.empty))
    case GetServerRef(friendName) =>
      val clientWhoAsked = (sender, senderName)
      findChatServerForMembers(clientWhoAsked._2, friendName)
        .ifSuccess(server => clientWhoAsked._1 ! ResponseForServerRefRequest(Option(server.head)))
        .orElse(_ => clientWhoAsked._1 ! ResponseForServerRefRequest(Option.empty))
    case GetGroupServerRef(groupName) =>
      model.findGroup(groupName)
        .ifSuccess(group => sender ! ResponseForServerRefRequest(Option(group.head)))
        .orElse(_ => sender ! ResponseForServerRefRequest(Option.empty))
  }

  def senderName: String = {
    var name = model.invalidName
    model.findUserName(sender).ifSuccess(n => name = n.head)
    name
  }

  def createNewOneToOneChat(friendName: String): EmptyOperationDone = {
    var result = false
    val clientWhoAsked = (sender, senderName)
    ifSenderRegisteredOrElse(() => model.findUser(friendName).ifSuccess(friendRef => {
      findChatServerForMembers(clientWhoAsked._2, friendName)
        .ifSuccess(_ => Unit)
        .orElse(_ => {
          val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], (clientWhoAsked._2, clientWhoAsked._1), (friendName, friendRef.head)))
          model.addNewOneToOneChatServer(newChatServer)
          result = true
        })
    }).orElse(_ => Unit), () => Unit)
    EmptyOperationDone(result)
  }

  def createNewGroupChat(newGroupName: String): EmptyOperationDone = {
    var success = false
    ifNewNameIsValidOrElse(newGroupName, () =>
      ifSenderRegisteredOrElse(() => model.findGroup(newGroupName).ifSuccess(_ => Unit).orElse(_ => {
        val newGroupChatServer = context.actorOf(Props(classOf[GroupChatServer], Map.empty.asInstanceOf[Map[String, ActorRef]], newGroupName))
        model.addNewGroupChatServer(newGroupName, newGroupChatServer)
        success = true
      }), () => Unit), () => Unit)
    EmptyOperationDone(success)
  }

  def findChatServerForMembers(senderName: String, friendName: String): OperationDone[ActorRef] = {
    val b = model.getChatsAsList match {
      case a if a.nonEmpty => Await.result(
        Future.sequence(a.map(chatServer => (chatServer, chatServer.ask(DoesContainsMembers(senderName, friendName)).mapTo[ContainsMembers]))
          .map(chatServerAndFuture => chatServerAndFuture._2.map(future => (chatServerAndFuture._1, future.trueOrFalse))))
          .map(responseList => responseList.find(chatServerAndResponse => chatServerAndResponse._2)), 10 seconds)
      case _ => Option.empty
    }
    OperationDone(b.isDefined, if (b.isDefined) List(b.get._1) else List.empty)
  }

  def findGroupChatServersContainingMembers(members: String*): OperationDone[List[ActorRef]] = {
    val allGroupChatServers = new ListBuffer[ActorRef]
    model.getAllUsersAndGroupsNames.ifSuccess(usersAndGroups => usersAndGroups.head._2.foreach(groupName => model.findGroup(groupName).ifSuccess(groupChatServer => allGroupChatServers += groupChatServer.head)))
    val groupChatServersContainingMembers = allGroupChatServers.toList match {
      case a if a.nonEmpty => Await.result(
        Future.sequence(a.map(chatServer => (chatServer, chatServer.ask(DoesContainsMembersInList(members.toList)).mapTo[ContainsMembers]))
          .map(chatServerAndFuture => chatServerAndFuture._2.map(future => (chatServerAndFuture._1, future.trueOrFalse))))
          .map(responseList => responseList.filter(chatServerAndResponse => chatServerAndResponse._2).map(refAndResult => refAndResult._1)), 10 seconds)
      case _ => List.empty
    }
    OperationDone(groupChatServersContainingMembers.nonEmpty, List(groupChatServersContainingMembers))
  }

  def sendNewServersToAllClients(): Unit =
    model.getAllUsersAndGroupsNames.ifSuccess(usersAndGroups => usersAndGroups.head._1.foreach(user => model.findUser(user).ifSuccess(user => self.tell(AllUsersAndGroupsRequest, user.head))))

  def unregisterUser(user: ActorRef): Unit = {
    var userName = model.invalidName
    model.findUserName(user).ifSuccess(name => userName = name.head)
    model.getAllUsersAndGroupsNames.ifSuccess(usersAndGroups => usersAndGroups.head._1.foreach(friend => {
      findChatServerForMembers(userName, friend).ifSuccess(chatServer => {
        model.removeOneToOneChatServer(chatServer.head)
        context.stop(chatServer.head)
      })
    }))
    val allGroupChatServers = new ListBuffer[ActorRef]
    model.getAllUsersAndGroupsNames.ifSuccess(usersAndGroups => usersAndGroups.head._2.foreach(groupName => model.findGroup(groupName).ifSuccess(groupChatServer => allGroupChatServers += groupChatServer.head)))
    allGroupChatServers.toList.foreach(group => group ! RemoveMember(userName))
    model.removeUser(userName)
  }

  def ifSenderRegisteredOrElse(ifPresent: () => Unit, ifNotPresent: () => Unit): Unit = {
    //fold(if_not_present)(if_present)
    model.getAllUsersAndGroupsNames
      .ifSuccess(usersAndGroups => usersAndGroups.head._1.find(_ == senderName).fold(ifNotPresent())(_ => ifPresent()))
      .orElse(_ => ifNotPresent())
  }
}

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class Unjoin()
  case class AllUsersAndGroupsRequest()
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest(newGroupName:String)
  case class GetServerRef(friendNname:String)
  case class GetGroupServerRef(groupName:String)
  case class ContainsMembers(trueOrFalse: Boolean)
}
