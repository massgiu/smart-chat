import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, ResponseForServerRefRequest, UserAndGroupActive}
import OneToOneChatServer.DoesContainsMembers
import RegisterServer._
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
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
    case JoinRequest(clientName) => addNewUserAndAnswer(clientName, sender)
    case Unjoin() => ifSenderRegisteredOrElse(() => users -= senderName, () => Unit)
    case NewGroupChatRequest(newGroupName) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false)
      ifNewNameIsValidOrElse(newGroupName, () =>
        ifSenderRegisteredOrElse(() => groups.ifPresentOrElse(newGroupName, () => {
          val newGroupChatServer = context.actorOf(Props(classOf[GroupChatServer], Set(sender)))
          groups += (newGroupName -> newGroupChatServer) //noSender will be substituted by the groupchatserver
          sender ! ResponseForChatCreation(accept = true)
        }, _ => onFail()), onFail), onFail)
    case AllUsersAndGroupsRequest =>
      ifSenderRegisteredOrElse(() => sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList), () => sender ! UserAndGroupActive(List.empty, List.empty))
    case NewOneToOneChatRequest(friendName) =>
      val clientWhoAsked = sender
      val localSenderName = senderName
      val onFail = () => clientWhoAsked ! ResponseForChatCreation(accept = false)
      ifSenderRegisteredOrElse(() => users.find(_._1 == friendName).fold(onFail())(_ => {
        findChatServerForMembers(localSenderName, friendName, _ => onFail(), () => {
          val friendRef = users(friendName)
          val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], (localSenderName, clientWhoAsked), (friendName, friendRef)))
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
      val localSenderName = senderName
      findChatServerForMembers(localSenderName, friendName, server => clientWhoAsked ! ResponseForServerRefRequest(Option(server)), () => clientWhoAsked ! ResponseForServerRefRequest(Option.empty))
  }

  def senderName: String = {
    users.find(_._2 == sender).map(u => u._1).get
  }

  def addNewUserAndAnswer(clientName: String, clientRef: ActorRef): Unit = {
    val onFail = () => clientRef ! AcceptRegistrationFromRegister(false)
    ifNewNameIsValidOrElse(clientName, () => users.find(_._1 == clientName).fold({
      users += (clientName -> clientRef)
      clientRef ! AcceptRegistrationFromRegister(true)
    })(_ => onFail()), onFail)
  }

  def findChatServerForMembers(senderName: String, friendName: String, ifFound: ActorRef => Any, ifNotFound: () => Unit): Any = {
    chats match {
      case a if a.nonEmpty =>
        Future.sequence(a.map(chatServer => (chatServer, chatServer.ask(DoesContainsMembers(senderName, friendName)).mapTo[ContainsMembers]))
          .map(chatServerAndFuture => chatServerAndFuture._2.map(future => (chatServerAndFuture._1, future.trueOrFalse))))
          .foreach(responseList => responseList.find(chatServerAndResponse => chatServerAndResponse._2)
            .fold(ifNotFound())(chatServerAndResponse => ifFound(chatServerAndResponse._1)))
      case _ => ifNotFound()
    }
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
