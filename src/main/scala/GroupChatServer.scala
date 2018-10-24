import Client.StringMessageFromServer
import GroupChatServer.{AddMember, GroupMessage, RemoveMember}
import akka.actor.{Actor, ActorRef}

class GroupChatServer(m: Set[ActorRef] = Set.empty) extends Actor {

  var members: Set[ActorRef] = m
  private var messageNumber: Long = 0

  override def receive: Receive =  {
    case AddMember(member) => members += member
    case RemoveMember(member) => members -= member
    case GroupMessage(text) =>
      messageNumber += 1
      members.foreach(m => m ! StringMessageFromServer(text, messageNumber, "")) //"" is wrong
    case _ => println("unknown message")
  }

}

object GroupChatServer {
  case class AddMember(member: ActorRef)
  case class RemoveMember(member: ActorRef)
  case class GroupMessage(text: String)
  case class GroupAttachment(payload:AttachmentContent)
}
