import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, UserAndGroupActive}
import RegisterServer._
import akka.actor.{Actor, ActorRef}

class RegisterServer extends Actor{

  var users: Map[String, ActorRef] = Map.empty
  var groups: Map[String, List[String]] = Map.empty

  override def receive: Receive = {
    case JoinRequest(clientName) =>
      users.find(_._1 == clientName).fold({
        users += (clientName -> sender)
        sender ! AcceptRegistrationFromRegister(true)
      })(_ => sender ! AcceptRegistrationFromRegister(false))
    case NewGroupChatRequest(newGroupName) =>
      groups.find(_._1 == newGroupName).fold({
        users += (newGroupName -> List.empty)
        sender ! AcceptRegistrationFromRegister(true)
      })(_ => sender ! AcceptRegistrationFromRegister(false))
    case AllUsersAndGroupsRequest =>
      sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList)
    case NewOneToOneChatRequest(friendName) =>
      //fold(if_not_present)(if_present)
      users.find(_._1 == friendName).fold(sender ! ResponseForChatCreation(false))(_ => sender ! ResponseForChatCreation(true))
    case JoinGroupChatRequest(group) =>
      groups.find(_._1 == group).fold(sender ! ResponseForChatCreation(false))(_ => sender ! ResponseForChatCreation(true))
  }
}

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class AllUsersAndGroupsRequest()
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest(newGroupName:String)
  case class JoinGroupChatRequest(group:String)
}
