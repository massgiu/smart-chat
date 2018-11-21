import Client.StringMessageFromServer
import GroupChatServer.{AddMember, GroupMessage, RemoveMember}
import akka.actor.{Actor, ActorRef}
import Utils._

class GroupChatServer(m: Map[String, ActorRef] = Map.empty) extends Actor {

  var members: Map[String, ActorRef] = m
  private var messageNumber: Long = 0

  override def receive: Receive =  {
    case AddMember(name: String, actRef : ActorRef) =>
      findInMap(name,members)
        .ifSuccess(_ => sender ! RegisterServer.ResponseFromJoinRequest(false))
        .orElse(value => {
          members += (name-> actRef)
          sender ! RegisterServer.ResponseFromJoinRequest(true)
        })
    case RemoveMember(name) =>
      findInMap(name,members)
        .ifSuccess(user => {
          members.filterKeys(_ != user)
          RegisterServer.ResponseFromUnJoinRequest(true)
        })
        .orElse(_ => sender ! RegisterServer.ResponseFromUnJoinRequest(false))
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
}
