import akka.actor.Actor

class Client extends Actor{

  override def receive: Receive = ???

}

object Client{

  /**
    * Represents the acceptance from server about client
    * registration request
    * @param accept response from server
    */
  final case class AcceptRegistrationFromRegister(accept: Boolean)

  /**
    * Represents the users list and active chat group
    * @param userList username list
    * @param groupList chat group list
    */
  final case class UserAndGroupActive(userList: List[String], groupList : List[String])

  /**
    * Represents a message sent from server console
    * @param message
    */
  final case class StringMessageFromServer(message : String)

  /**
    * Represents a message sent from client console
    * @param message message sent
    */
  final case class StringMessageFromConsole(message : String)

  /**
    * Represents an attachment sent from server
    * @param payload attachment sent
    */
  final case class AttachmentMessageFromServer(payload : Attachment)

  /**
    * Represents an attachment sent from client console
    * @param payload attachment sent
    */
  final case class AttachmentMessageFromConsole(payload : Attachment)

  /**
    * Represents the response from Server about client request
    * for chat creation
    * @param accept response from server
    */
  final case class ResponseForChatCreation(accept : Boolean)
}
