import Client.{AttachmentMessageFromServer, StringMessageFromGroupServer, StringMessageFromServer}
import GroupChatServer._
import RegisterServer.ContainsMembers
import akka.actor.{Actor, ActorRef}
import Utils._

class GroupChatServer(m: Map[String, ActorRef] = Map.empty, groupName: String) extends Actor {

  var members: Map[String, ActorRef] = m
  private var messageNumber: Long = 0

  override def receive: Receive =  {
    case AddMember(name: String, actRef : ActorRef) =>
      findInMap(name,members)
        .ifSuccess(_ => sender ! Client.ResponseForJoinGroupRequest(accept = false,groupName))
        .orElse(_ => {
          members += (name-> actRef)
          sender ! Client.ResponseForJoinGroupRequest(accept = true,groupName)
        })
    case RemoveMember(name) =>
      findInMap(name,members)
        .ifSuccess(_ => {
          members -= name
          sender ! Client.ResponseForUnJoinGroupRequest(accept = true,groupName)
        })
        .orElse(_ => sender ! Client.ResponseForUnJoinGroupRequest(accept = false,groupName))
    case GroupMessage(text, senderName) =>
      messageNumber += 1
      members.foreach(member => member._2 ! StringMessageFromGroupServer(text, messageNumber, senderName, groupName))
    case GroupAttachment(payload, senderName) =>
      messageNumber += 1
      members.foreach(member => member._2 ! AttachmentMessageFromServer(payload : Array[Byte], messageNumber: Long, senderName, groupName))
    case DoesContainsMembersInList(users) =>
      sender ! ContainsMembers(users.toSet.subsetOf(members.keySet))
    case _ => println("unknown message")
  }
}

object GroupChatServer {
  case class AddMember(name: String, actRef: ActorRef)
  case class RemoveMember(name: String)
  case class GroupMessage(text: String, senderName: String)
  case class GroupAttachment(payload:Array[Byte],senderName: String)
  case class JoinGroupChatRequest(name: String)
  case class UnJoinGroupChatRequest(name: String)
  case class DoesContainsMembersInList(users: List[String])
}
