import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, ResponseForServerRefRequest, UserAndGroupActive}
import OneToOneChatServer.DoesContainsMembers
import RegisterServer._
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import MapExtension._

class RegisterServer extends Actor{

  //futures' callbacks will be executed on the messageDispatcher, which is an executionContext
  import context.dispatcher

  implicit val timeout: Timeout = Timeout(5 seconds)
  var users: Map[String, ActorRef] = Map.empty
  var groups: Map[String, ActorRef] = Map.empty
  var chats: List[ActorRef] = List.empty

  override def receive: Receive = {
    case JoinRequest(clientName) => addNewUser(clientName, sender)
      .ifSuccess(_ => sender ! AcceptRegistrationFromRegister(true))
      .orElse(_ => sender ! AcceptRegistrationFromRegister(false))
    case Unjoin() => removeUser(senderName)
    case NewGroupChatRequest(newGroupName) => createNewGroupChat(newGroupName)
        .ifSuccess(_ => sender ! ResponseForChatCreation(true))
        .orElse(_ => sender ! ResponseForChatCreation(false))
    case AllUsersAndGroupsRequest =>
      ifSenderRegisteredOrElse(() => sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList), () => sender ! UserAndGroupActive(List.empty, List.empty))
    case NewOneToOneChatRequest(friendName) =>
      val clientWhoAsked = sender
      val clientWhoAskedName = senderName
      val onFail = () => clientWhoAsked ! ResponseForChatCreation(accept = false)
      ifSenderRegisteredOrElse(() => users.find(_._1 == friendName).fold(onFail())(_ => {
        findChatServerForMembers(clientWhoAskedName, friendName)
          .ifSuccess(_ => onFail())
          .orElse(_ => {
            val friendRef = users(friendName)
            val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], (clientWhoAskedName, clientWhoAsked), (friendName, friendRef)))
            chats = newChatServer :: chats
            clientWhoAsked ! ResponseForChatCreation(accept = true)
          })
      }), onFail)
    case JoinGroupChatRequest(group) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false)
      ifSenderRegisteredOrElse(() => groups.find(_._1 == group).fold(onFail())(_ => {
        sender ! ResponseForChatCreation(accept = true)
      }), onFail)
    case GetServerRef(friendName) =>
      val clientWhoAsked = sender
      val clientWhoAskedName = senderName
      findChatServerForMembers(clientWhoAskedName, friendName)
        .ifSuccess(server => clientWhoAsked ! ResponseForServerRefRequest(Option(server.head)))
        .orElse(_ => clientWhoAsked ! ResponseForServerRefRequest(Option.empty))
  }

  def senderName: String = {
    users.find(_._2 == sender).map(u => u._1).get
  }

  def addNewUser(clientName: String, clientRef: ActorRef): EmptyOperationDone = {
    var success = false
    ifNewNameIsValidOrElse(clientName, () => users.find(_._1 == clientName).fold({
      users += (clientName -> clientRef)
      success = true
    })(_ => Unit), () => Unit)
    EmptyOperationDone(success)
  }

  def removeUser(clientName: String): Unit = {
    ifSenderRegisteredOrElse(() => users -= senderName, () => Unit)
  }

  def getAllUsersAndGroups: (List[String], List[String]) = {
    (users.keys.toList, groups.keys.toList)
  }

  def createNewGroupChat(newGroupName: String): EmptyOperationDone = {
    var success = false
    ifNewNameIsValidOrElse(newGroupName, () =>
      ifSenderRegisteredOrElse(() => groups.ifPresentOrElse(newGroupName, () => {
        val newGroupChatServer = context.actorOf(Props(classOf[GroupChatServer], Set(sender)))
        groups += (newGroupName -> newGroupChatServer)
        success = true
      }, _ => Unit), () => Unit), () => Unit)
    EmptyOperationDone(success)
  }

  def findChatServerForMembers(senderName: String, friendName: String): OperationDone[ActorRef] = {
    val b = chats match {
      case a if a.nonEmpty => Await.result(
        Future.sequence(a.map(chatServer => (chatServer, chatServer.ask(DoesContainsMembers(senderName, friendName)).mapTo[ContainsMembers]))
          .map(chatServerAndFuture => chatServerAndFuture._2.map(future => (chatServerAndFuture._1, future.trueOrFalse))))
          .map(responseList => responseList.find(chatServerAndResponse => chatServerAndResponse._2)), 10 seconds)
      case _ => Option.empty
    }
    OperationDone(b.isDefined, if (b.isDefined) List(b.get._1) else List.empty)
  }

  def ifSenderRegisteredOrElse(ifPresent: () => Unit, ifNotPresent: () => Unit): Unit = {
    //fold(if_not_present)(if_present)
    users.values.find(_ == sender).fold(ifNotPresent())(_ => ifPresent())
  }

  def ifNewNameIsValidOrElse(name: String, ifValid: () => Unit, ifNotValid: () => Unit): Unit = {
    if (name != null && name.length > 0)
      ifValid()
    else
      ifNotValid()
  }
}

class OperationDone[A](success: Boolean, result: List[A]) {
  def ifSuccess(action: List[A] => Unit): OperationDone[A] = {if(success) action(result); this}
  def orElse(action: List[A] => Unit): Unit = if(!success) action(result)
}

object OperationDone {
  def apply[A](success: Boolean, result: List[A] = List.empty): OperationDone[A] = new OperationDone(success, result)
}

class EmptyOperationDone(success: Boolean) extends OperationDone[Unit](success, List.empty)

object EmptyOperationDone {
  def apply(success: Boolean): EmptyOperationDone = new EmptyOperationDone(success)
}

object MapExtension {
  implicit class ExtendedMap(m: Map[String, ActorRef]) {
    def ifPresentOrElse(key: String, ifNotPresent: () => Unit, ifPresent: ((String, ActorRef)) => Unit): Unit = m.find(keyValue => keyValue._1 == key).fold(ifNotPresent())(ifPresent)
  }
}

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class Unjoin()
  case class AllUsersAndGroupsRequest()
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest(newGroupName:String)
  case class JoinGroupChatRequest(group:String)
  case class GetServerRef(friendNname:String)
  case class ContainsMembers(trueOrFalse: Boolean)
}
