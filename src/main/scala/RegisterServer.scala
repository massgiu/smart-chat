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
      groups.find(_._1 == newGroupName).fold({
        groups += (newGroupName -> ActorRef.noSender)
        sender ! AcceptRegistrationFromRegister(true)
      })(_ => sender ! AcceptRegistrationFromRegister(false))
    case AllUsersAndGroupsRequest =>
      sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList)
    case NewOneToOneChatRequest(friendName) =>
      //fold(if_not_present)(if_present)
      users.find(_._1 == friendName).fold(sender ! ResponseForChatCreation(accept = false, Option.empty))(_ => {
        val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], sender, users(friendName)), name = "welcomeServer1")
        chats = newChatServer :: chats
        sender ! ResponseForChatCreation(accept = true, Option(newChatServer))
      })
    case JoinGroupChatRequest(group) =>
      groups.find(_._1 == group).fold(sender ! ResponseForChatCreation(accept = false, Option.empty))(_ => {
        sender ! ResponseForChatCreation(accept = true, Option(groups(group)))
      })
  }
}

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class AllUsersAndGroupsRequest()
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest(newGroupName:String)
  case class JoinGroupChatRequest(group:String)
}
