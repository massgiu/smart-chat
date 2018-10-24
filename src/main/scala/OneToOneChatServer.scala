import Client.StringMessageFromServer
import OneToOneChatServer.{DoesContainsMembers, Message}
import RegisterServer.ContainsMembers
import akka.actor.{Actor, ActorRef}

class OneToOneChatServer(one: (String, ActorRef), two: (String, ActorRef)) extends Actor {

  val memberOne: (String, ActorRef) = one
  val memberTwo: (String, ActorRef) = two
  private var messageNumber: Long = 0

  override def receive: Receive = {
    case Message(text) =>
      messageNumber += 1
      memberOne._2 ! StringMessageFromServer(text, messageNumber, senderName)
      memberTwo._2 ! StringMessageFromServer(text, messageNumber, senderName)
    case DoesContainsMembers(firstMemberName, secondMemberName) =>
      if ((memberOne._1 == firstMemberName && memberTwo._1 == secondMemberName) || (memberTwo._1 == firstMemberName && memberOne._1 == secondMemberName))
        sender ! ContainsMembers(self, trueOrFalse = true)
      else
        sender ! ContainsMembers(self, trueOrFalse = false)
    case _ => println("unknown message")
  }

  def senderName: String = {
    if (sender == memberOne._2) memberOne._1 else memberTwo._1
  }

}

object OneToOneChatServer {

  case class Message(text:String)
  case class Attachment(payload:AttachmentContent)
  case class DoesContainsMembers(firstMemberName: String, secondMemberName: String)
}
