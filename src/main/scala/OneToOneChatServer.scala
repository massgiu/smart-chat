import Client.{AttachmentMessageFromServer, StringMessageFromServer}
import OneToOneChatServer.{Attachment, DoesContainsMembers, Message}
import RegisterServer.ContainsMembers
import akka.actor.{Actor, ActorRef}

class OneToOneChatServer(one: (String, ActorRef), two: (String, ActorRef)) extends Actor {

  val memberOne: (String, ActorRef) = one
  val memberTwo: (String, ActorRef) = two
  private var messageNumber: Long = 0

  override def receive: Receive = {
    case Message(text) =>
      messageNumber += 1
      memberOne._2 ! StringMessageFromServer(text, messageNumber, senderName, recipientName)
      memberTwo._2 ! StringMessageFromServer(text, messageNumber, senderName, recipientName)
    case DoesContainsMembers(firstMemberName, secondMemberName) =>
      if ((memberOne._1 == firstMemberName && memberTwo._1 == secondMemberName) || (memberTwo._1 == firstMemberName && memberOne._1 == secondMemberName))
        sender ! ContainsMembers(trueOrFalse = true)
      else
        sender ! ContainsMembers(trueOrFalse = false)
    case Attachment(payload: Array[Byte],sender: String, recipient: String) =>
      messageNumber += 1
      memberOne._2 ! AttachmentMessageFromServer(payload, messageNumber, senderName, recipientName)
      memberTwo._2 ! AttachmentMessageFromServer(payload, messageNumber, senderName, recipientName)
    case _ => println("unknown message")
  }

  def senderName: String = {
    if (sender == memberOne._2) memberOne._1 else memberTwo._1
  }

  def recipientName: String = {
    if (sender == memberOne._2) memberTwo._1 else memberOne._1
  }

}

object OneToOneChatServer {

  case class Message(text:String)
  case class Attachment(payload:Array[Byte],sender : String, recipient : String)
  case class DoesContainsMembers(firstMemberName: String, secondMemberName: String)
}
