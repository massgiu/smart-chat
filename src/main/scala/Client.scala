import Client._
import OneToOneChatServer.{Attachment, Message}
import RegisterServer.{AllUsersAndGroupsRequest, JoinGroupChatRequest, JoinRequest, NewGroupChatRequest}
import akka.actor.{Actor, ActorRef, ActorSelection, Props}

class Client extends Actor{

  val register_address : String = new String
  val register : ActorSelection  = context.actorSelection(register_address)
  var chatServer : ActorRef = _

    override def preStart():Unit = {
      register.tell(JoinRequest("TestName"),self)
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
        * Display data on console
        */
    }

    case StringMessageFromServer(message) => {
      /**
        * Display data on console
        */
    }

    case StringMessageFromConsole(message) => {
      chatServer.tell(Message(message),self)
    }

    case AttachmentMessageFromServer(attachment : OneToOneChatServer.Attachment) =>{
      /** 
        * Display data on console
        */
    }

    case AttachmentMessageFromConsole(attachment : OneToOneChatServer.Attachment) =>{
      /**
        * Sends data to OneToOneChatServer
        */
      chatServer.tell(Attachment(attachment.payload),self)
    }

    case CreateGroupRequestFromConsole(groupName : String) => {
      register.tell(NewGroupChatRequest(groupName),self)
    }

    case JoinGroupRequestFromConsole(groupName : String) => {
      register.tell(JoinGroupChatRequest(groupName),self)
    }

    case ResponseForChatCreation(response,serverForChat) => {
      response match {
        case true => chatServer = serverForChat.get
        case  _ => println("Chat creation refused!")
      }
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
    * @param message
    */
  final case class StringMessageFromServer(message : String)

  /**
    * A message sent from client console
    * @param message message sent
    */
  final case class StringMessageFromConsole(message : String)

  /**
    * An attachment sent from server
    * @param payload attachment sent
    */
  final case class AttachmentMessageFromServer(payload : Attachment)

  /**
    * An attachment sent from client console
    * @param payload attachment sent
    */
  final case class AttachmentMessageFromConsole(payload : Attachment)

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
  final case class ResponseForChatCreation(accept : Boolean, chatServer : Option[ActorRef])

}
