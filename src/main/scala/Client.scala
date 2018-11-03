import java.io.File

import Client._
import OneToOneChatServer.{Attachment, Message}
import RegisterServer._
import akka.actor.{Actor, ActorRef, ActorSelection, ExtendedActorSystem, Props, Stash}
import com.typesafe.config.ConfigFactory

class Client(system: ExtendedActorSystem) extends Actor with Stash{

  var users: List[String] = List()
  var groups: List[String] = List()
  var userRefMap: Map[String, ActorRef] = Map.empty
  val registerFilePath : String = "src/main/scala/res/server.conf"

  var register : ActorSelection  = _
  var chatServer : ActorRef = _

    override def preStart():Unit = {
      val serverConfig = ConfigFactory.parseFile(new File(registerFilePath))
      val hostname = serverConfig.getAnyRef("akka.remote.netty.tcp.hostname")
      val port = serverConfig.getAnyRef("akka.remote.netty.tcp.port")
      register = context.actorSelection("akka.tcp://MySystem@"+hostname+":"+port+"/user/server")
      println("New Client @: " + self.path + "/user/server" + " started!")
      //Application.launch(classOf[LaunchClientLogin],self.toString())
      register ! JoinRequest("testB")
    }

    override def receive: Receive = {

    case AcceptRegistrationFromRegister(response) => {
      response match {
        case true => {
          println("Connection accepted from server")
          sender.tell(AllUsersAndGroupsRequest,self)
        }
        case  _ => println("Connection refused")
      }
    }
    case UserAndGroupActive(userList, groupList)=> {
      users = userList
      groups = groupList
    }
    case StringMessageFromServer(message, messageNumber,senderName) => {
      /**
        * Display data on view/console
        */
      println(message + " from " + senderName)
    }
    case StringMessageFromConsole(message, senderName) => {
      userRefMap.find(nameAndRef=>nameAndRef._1==senderName).fold({
        register ! GetServerRef(senderName)
        unstashAll()
        context.become({
          case ResponseForServerRefRequest(chatServer) => chatServer match {
            case Some(serverRef)=> {
              userRefMap += (senderName -> chatServer.get)
              unstashAll()
              context.unbecome()
              self ! StringMessageFromConsole(message, senderName)
            }
            case _=> {
              println("ChatServer unreachable")
              unstashAll()
              context.unbecome()
            }
          }
          case _ => stash()
        }, discardOld = false) // stack on top instead of replacing

      })(nameAndRef => nameAndRef._2 ! Message(message))
    }
    case AttachmentMessageFromServer(attachment, userName, messageNumber) =>{
      /**
        * Display data on view/console
        */
    }
    case AttachmentMessageFromConsole(attachment, userName) =>{
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
        case true => {
          println("Chat creation done!")
          /**
            * Sends data to console
            */
        }
        case  _ => println("Chat creation refused!")
      }
    }

    case ResponseForServerRefRequest(actref) => {
      actref match {
        case None => println("No ref value")
        case _ => println(actref.get)
      }
    }

    case LogInFromConsole(userName) => userName match {
      case username: String if username.length>0 => {
        register ! JoinRequest(userName)
      }
      case _ => println("Invalid username")
    }
    case RequestForChatCreationFromConsole(friendName) => {
      users.find(user => user==friendName).fold({
      register ! AllUsersAndGroupsRequest
      unstashAll()
      context.become({
        case UserAndGroupActive(userList, groupList)=> {
          users = userList
          groups = groupList
          unstashAll()
          context.unbecome()
          self ! RequestForChatCreationFromConsole(friendName)
        }
        case _ => stash()
      }, discardOld = false) // stack on top instead of replacing
      })(user => register ! NewOneToOneChatRequest(user))
    }
  }
}

object Client{

  /**
    * Get response about acceptance from server about client
    * registration request
    * @param accept response from server
    */
  final case class AcceptRegistrationFromRegister(accept: Boolean)

  /**
    * Get users list and active chat group list
    * @param userList username list
    * @param groupList chat group list
    */
  final case class UserAndGroupActive(userList: List[String], groupList : List[String])

  /**
    * Get a message sent from server console
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
    * Get a message sent from client console
    * @param message message sent
    */
  final case class StringMessageFromConsole(message : String, recipient : String)

  /**
    * Get an attachment sent from server
    * @param payload attachment sent
    * @param messageNumber the progressive number used to order all the exchanged messages
    */
  final case class AttachmentMessageFromServer(payload : AttachmentContent, messageNumber: Long, sender : String)

  /**
    * Get an attachment sent from client console
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
    * Get response from server about the reference of an oneToOne or group chat
    * @param actRef
    */
  final case class ResponseForServerRefRequest(actRef : Option[ActorRef])

  /**
    * Get username from LoginController
    * @param userName
    */
  final case class LogInFromConsole(userName:String)

  /**
    * Request to create a one to one chat from client console
    * @param friendName username to chat with
    */
  final case class RequestForChatCreationFromConsole(friendName : String)

}
