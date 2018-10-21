import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, UserAndGroupActive}
import RegisterServer._
import akka.actor.{Actor, ActorRef, Props}

class RegisterServer extends Actor{

  var users: Map[String, ActorRef] = Map.empty
  var groups: Map[String, ActorRef] = Map.empty
  var chats: List[ActorRef] = List.empty

  override def receive: Receive = {
    case JoinRequest(clientName) =>
      users.find(_._1 == clientName).fold({
        users += (clientName -> sender)
        sender ! AcceptRegistrationFromRegister(true)
      })(_ => sender ! AcceptRegistrationFromRegister(false))
    case NewGroupChatRequest(newGroupName) =>
      val onFail = () => sender ! AcceptRegistrationFromRegister(false)
      ifSenderPresentOrElse(() => groups.find(_._1 == newGroupName).fold({
        groups += (newGroupName -> ActorRef.noSender) //noSender will be substituted by the groupchatserver
        sender ! AcceptRegistrationFromRegister(true)
      })(_ => onFail()), onFail)
    case AllUsersAndGroupsRequest =>
      ifSenderPresentOrElse(() => sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList), () => sender ! UserAndGroupActive(List.empty, List.empty))
    case NewOneToOneChatRequest(friendName) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false, Option.empty)
      ifSenderPresentOrElse(() => users.find(_._1 == friendName).fold(onFail())(_ => {
        val friendRef = users(friendName)
        val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], sender, friendRef), name = "OneToOne" + friendName)
        chats = newChatServer :: chats
        sender ! ResponseForChatCreation(accept = true, Option(newChatServer))
      }), onFail)
    case JoinGroupChatRequest(group) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false, Option.empty)
      ifSenderPresentOrElse(() => groups.find(_._1 == group).fold(onFail())(_ => {
        sender ! ResponseForChatCreation(accept = true, Option(groups(group)))
      }), onFail)
  }

  def ifSenderPresentOrElse(ifPresent: () => Unit, ifNotPresent: () => Unit): Unit = {
    //fold(if_not_present)(if_present)
    users.values.find(_ == sender).fold(ifNotPresent())(_ => ifPresent())
  }
}

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class AllUsersAndGroupsRequest()
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest(newGroupName:String)
  case class JoinGroupChatRequest(group:String)
}
