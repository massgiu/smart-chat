import akka.actor.Actor

class RegisterServer extends Actor{
  override def receive: Receive = ???
}

object RegisterServer {
  case class JoinRequest(clientName:String)
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest()
  case class JoinGroupChatRequest(group:String)
}
