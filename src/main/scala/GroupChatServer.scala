import Client.{StringMessageFromServer}
import GroupChatServer._
import akka.actor.{Actor, ActorRef}
import Utils._

class GroupChatServer(m: Map[String, ActorRef] = Map.empty, groupName: String) extends Actor {

  var members: Map[String, ActorRef] = m
  private var messageNumber: Long = 0

  override def receive: Receive =  {
    case AddMember(name: String, actRef : ActorRef) =>
      findInMap(name,members)
        .ifSuccess(_ => sender ! Client.ResponseForJoinGroupRequest(false,groupName))
        .orElse(value => {
          members += (name-> actRef)
          sender ! Client.ResponseForJoinGroupRequest(true,groupName)
        })
    case RemoveMember(name) =>
      findInMap(name,members)
        .ifSuccess(user => {
          members.filterKeys(_ != user)
          Client.ResponseForUnJoinGroupRequest(true,groupName)
        })
        .orElse(_ => sender ! Client.ResponseForUnJoinGroupRequest(false,groupName))
    case GroupMessage(text, senderName) =>
      messageNumber += 1
      members.keys.foreach(name => members(name) ! StringMessageFromServer(text, messageNumber, senderName,name))
    case _ => println("unknown message")
  }
}

object GroupChatServer {
  case class AddMember(name: String, actRef: ActorRef)
  case class RemoveMember(name: String)
  case class GroupMessage(text: String, senderName : String)
  case class GroupAttachment(payload:AttachmentContent)
  case class JoinGroupChatRequest(name: String)
  case class UnJoinGroupChatRequest(name: String)
}
