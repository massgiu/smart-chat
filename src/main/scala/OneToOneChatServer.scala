import OneToOneChatServer.Message
import akka.actor.{Actor, ActorRef}

class OneToOneChatServer(one: ActorRef, two: ActorRef) extends Actor {

  val memberOne: ActorRef = one
  val memberTwo: ActorRef = two

  override def receive: Receive = {
    case Message(text) => sender() match {
      case `memberOne` => memberTwo ! Message(text)
      case `memberTwo` => memberOne ! Message(text)
    }
    case _ => println("unknown message")
  }

}

object OneToOneChatServer {

  case class Message(text:String)
  case class Attachment(payload:AttachmentContent)
}

class AttachmentContent {

}
