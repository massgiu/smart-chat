import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, UserAndGroupActive}
import RegisterServer._
import akka.actor.{Actor, ActorRef, Props}

class RegisterServer extends Actor{

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
      val onFail = () => sender ! ResponseForChatCreation(accept = false, Option.empty)
      ifNewNameIsValidOrElse(newGroupName, () =>
        ifSenderPresentOrElse(() => groups.find(_._1 == newGroupName).fold({
          val newGroupChatServer = context.actorOf(Props(classOf[GroupChatServer], Set(sender)))
          groups += (newGroupName -> newGroupChatServer) //noSender will be substituted by the groupchatserver
          sender ! ResponseForChatCreation(accept = true, Option(newGroupChatServer))
        })(_ => onFail()), onFail), onFail)
    case AllUsersAndGroupsRequest =>
      ifSenderPresentOrElse(() => sender ! UserAndGroupActive(users.keys.toList, groups.keys.toList), () => sender ! UserAndGroupActive(List.empty, List.empty))
    case NewOneToOneChatRequest(friendName) =>
      val onFail = () => sender ! ResponseForChatCreation(accept = false, Option.empty)
      ifSenderPresentOrElse(() => users.find(_._1 == friendName).fold(onFail())(_ => {
        val friendRef = users(friendName)
        val newChatServer = context.actorOf(Props(classOf[OneToOneChatServer], sender, friendRef))
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
}
