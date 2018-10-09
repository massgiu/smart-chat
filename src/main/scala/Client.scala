import akka.actor.Actor

class Client extends Actor{

  override def receive: Receive = ???

}

object Client{

  //Message from server
  final case class AcceptRegistrationFromRegister(accept: Boolean)

  final case class UserAndGroupActive(userList: List[String],groupList : List[String])

  final case class StringMessage(message : String)

  final case class AttachmentMessage()

  final case class ResponseForChatCreation(accept : Boolean)
}
