import Client._
import OneToOneChatServer.{Attachment, Message}
import RegisterServer.{AllUsersAndGroupsRequest, JoinGroupChatRequest, JoinRequest, NewGroupChatRequest}
import akka.actor.{Actor, ActorRef, ActorSelection, Props}

class Client extends Actor{

  val register_address : String = new String
  val register : ActorSelection  = context.actorSelection(register_address)
  var chatServer : ActorRef = _
  var myUserName : String = "TestName"

    override def preStart():Unit = {
      register.tell(JoinRequest(myUserName),self)
    }

    override def receive: Receive = {

    case AcceptRegistrationFromRegister(response) => {
      response match {
        case true => sender.tell(AllUsersAndGroupsRequest,self)
        case  _ => println("Connection refused")
      }
    }

    case UserAndGroupActive(userList, groupList)=> {
      /**
        * Display data on view/console
        */
    }

    case StringMessageFromServer(message, userName,messageNumber) => {
      /**
        * Display data on view/console
        */
    }

    case StringMessageFromConsole(message, userName) => {
      chatServer.tell(Message(message),self)
    }

    case AttachmentMessageFromServer(attachment, userName, messageNumber) =>{
      /**
        * Display data on view/console
        */
    }

    case AttachmentMessageFromConsole(attachment, userName) =>{
      /**
        * Sends data to OneToOneChatServer
        */
       chatServer.tell(Attachment(attachment),self)
    }

    case CreateGroupRequestFromConsole(groupName : String) => {
      register.tell(NewGroupChatRequest(groupName),self)
    }

    case JoinGroupRequestFromConsole(groupName : String) => {
      register.tell(JoinGroupChatRequest(groupName),self)
    }

    case ResponseForChatCreation(response) => {
      response match {
        case true => println("Chat creation done!")
        case  _ => println("Chat creation refused!")
      }
    }

    case ResponseForServerRefRequest(actref) => {
      chatServer = actref
    }
  }

}

object Client{

  /**
    * Response about acceptance from server about client
    * registration request
    * @param accept response from server
    */
  final case class AcceptRegistrationFromRegister(accept: Boolean)

  /**
    * Users list and active chat group list
    * @param userList username list
    * @param groupList chat group list
    */
  final case class UserAndGroupActive(userList: List[String], groupList : List[String])

  /**
    * A message sent from server console
    * @param message attachment sent
    * @param messageNumber the progressive number used to order all the exchanged messages
    */
  final case class StringMessageFromServer(message: String, messageNumber: Long, sender : String)

  /**
    *
    * @param message
    * @param messageNumber
    * @param sender
    * @param group
    */
  final case class StringMessageFromGroupServer(message: String, messageNumber: Long, sender : String, group: String)

  /**
    * A message sent from client console
    * @param message message sent
    */
  final case class StringMessageFromConsole(message : String, recipient : String)

  /**
    * An attachment sent from server
    * @param payload attachment sent
    * @param messageNumber the progressive number used to order all the exchanged messages
    */
  final case class AttachmentMessageFromServer(payload : AttachmentContent, messageNumber: Long, sender : String)

  /**
    * An attachment sent from client console
    * @param payload attachment sent
    */
  final case class AttachmentMessageFromConsole(payload : AttachmentContent, recipient : String)

  /**
    *
    * @param payload
    * @param messageNumber
    * @param sender
    * @param group
    */
  final case class AttachmentMessageFromGroupServer(payload: AttachmentContent, messageNumber: Long, sender : String, group: String)

  /**
    * Request to create a chat a group from client console
    */
  final case class CreateGroupRequestFromConsole(groupName : String)

  /**
    * Request to join to chat a group from client console
    * @param groupName name of the group
    */
  final case class JoinGroupRequestFromConsole(groupName:String)

  /**
    * Response from server about client request
    * for chat creation
    * @param accept response from server
    */
  final case class ResponseForChatCreation(accept : Boolean)

  /**
    * Response from server about the reference of an oneToOne or group chat
    * @param actRef
    */
  final case class ResponseForServerRefRequest(actRef : Option[ActorRef])

}
