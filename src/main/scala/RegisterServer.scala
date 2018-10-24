import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, ResponseForServerRefRequest, UserAndGroupActive}
import OneToOneChatServer.DoesContainsMembers
import RegisterServer._
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Success

class RegisterServer extends Actor{

  //futures' callbacks will be executed on the messageDispatcher, which is an executionContext
  import context.dispatcher

  implicit val timeout: Timeout = Timeout(5 seconds)
  var users: Map[String, ActorRef] = Map.empty
  var groups: Map[String, ActorRef] = Map.empty
  var chats: List[ActorRef] = List.empty

  override def receive: Receive = {
    case JoinRequest(clientName) =>
      val onFail = () => sender ! AcceptRegistrationFromRegister(false)
      ifNewNameIsValidOrElse(clientName, () => users.find(_._1 == clientName).fold({
        users += (clientName -> sender)
        sender ! AcceptRegistrationFromRegister(true)
      })(_ => onFail()), onFail)
    case NewGroupChatRequest(newGroupName) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false)
      ifNewNameIsValidOrElse(newGroupName, () =>
        ifSenderRegisteredOrElse(() => groups.find(_._1 == newGroupName).fold({
          val newGroupChatServer = context.actorOf(Props(classOf[GroupChatServer], Set(sender)))
          groups += (newGroupName -> newGroupChatServer) //noSender will be substituted by the groupchatserver
          sender ! ResponseForChatCreation(accept = true)
        })(_ => onFail()), onFail), onFail)
    case AllUsersAndGroupsRequest =>
      ifSenderRegisteredOrElse(() => sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList), () => sender ! UserAndGroupActive(List.empty, List.empty))
    case NewOneToOneChatRequest(friendName) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false)
      ifSenderRegisteredOrElse(() => users.find(_._1 == friendName).fold(onFail())(_ => {
        val friendRef = users(friendName)
        val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], (senderName, sender), (friendName, friendRef)))
        chats = newChatServer :: chats
        sender ! ResponseForChatCreation(accept = true)
      }), onFail)
    case JoinGroupChatRequest(group) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false)
      ifSenderRegisteredOrElse(() => groups.find(_._1 == group).fold(onFail())(_ => {
        sender ! ResponseForChatCreation(accept = true)
      }), onFail)
    case GetServerRef(friendName) =>
      val clientWhoAsked = sender
      findChatServerForMembers(senderName, friendName, server => clientWhoAsked ! ResponseForServerRefRequest(Option(server)))
  }

  def senderName: String = {
    users.find(_._2 == sender).map(u => u._1).get
  }

  def findChatServerForMembers(senderName: String, friendName: String, ifFound: ActorRef => Any): Any = {
    chats.map(c => c.ask(DoesContainsMembers(senderName, friendName)).mapTo[ContainsMembers])
      .foreach(future => future.onComplete({
        case Success(result) => result match {
          case ContainsMembers(server, true) => ifFound(server)
        }
      }))
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

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class AllUsersAndGroupsRequest()
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest(newGroupName:String)
  case class JoinGroupChatRequest(group:String)
  case class GetServerRef(friendNname:String)
  case class ContainsMembers(server: ActorRef, trueOrFalse: Boolean)
}
