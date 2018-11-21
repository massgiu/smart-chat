import java.io.File

import ActorLoginController.ResponseFromLogin
import Client._
import OneToOneChatServer.Message
import RegisterServer._
import akka.actor.{Actor, ActorRef, ActorSelection, ExtendedActorSystem, Stash}
import com.typesafe.config.ConfigFactory
import Utils._
import akka.actor.TypedActor.PostStop

class Client(system: ExtendedActorSystem) extends Actor with Stash with PostStop {

  var users: List[String] = List()
  var groups: List[String] = List()
  var userRefMap: Map[String, ActorRef] = Map.empty
  val registerFilePath: String = "src/main/scala/res/server.conf"
  var userName: String = _
  var storyMessageChat: Map[String, List[Option[StringMessageFromServer]]] = Map.empty //key is recipient

  var register: ActorSelection = _
  var actorView: Option[ActorRef] = Option.empty
  var actorLogin: Option[ActorRef] = Option.empty
  var messageRecipient: String = new String()

  override def preStart(): Unit = {
    val serverConfig = ConfigFactory.parseFile(new File(registerFilePath))
    val hostname = serverConfig.getAnyRef("akka.remote.netty.tcp.hostname")
    val port = serverConfig.getAnyRef("akka.remote.netty.tcp.port")
    register = context.actorSelection("akka.tcp://MySystem@" + hostname + ":" + port + "/user/server")
    println("New Client @: " + self.path + " started!")
  }

  override def receive: Receive = {

    case AcceptRegistrationFromRegister(response) =>
      if (response) {
        println("Connection accepted from server ")
        actorLogin.foreach(actor => actor ! ResponseFromLogin(true))
        sender ! AllUsersAndGroupsRequest
      } else {
        println("Connection refused ")
        actorLogin.foreach(actor => actor ! ResponseFromLogin(false))
      }
    case UserAndGroupActive(userList, groupList) =>
      users = userList
      groups = groupList
      actorView.foreach(actor => actor ! ActorViewController.UpdateUserAndGroupActive(users, groups))
    case StringMessageFromServer(message, messageNumber, senderName, recipientName) =>
      val recipient = if (userName != senderName) senderName else recipientName
      findInMap(recipient, storyMessageChat).ifSuccess(messagesList => {
        var temp = messagesList.head.toArray
        if (messageNumber > temp.head.get.messageNumber) {
          for (_ <- temp.head.get.messageNumber + 1 until messageNumber) {
            temp = Option.empty +: temp
          }
          temp = Option(StringMessageFromServer(message, messageNumber, senderName, recipient)) +: temp
        } else {
          temp(temp.length - messageNumber.toInt) = Option(StringMessageFromServer(message, messageNumber, senderName, recipient))
        }
        storyMessageChat += (recipient -> temp.toList)
      }).orElse(_ => storyMessageChat += (recipient -> List(Option(StringMessageFromServer(message, messageNumber, senderName, recipient)))))
      val toSend = storyMessageChat
        .map(friendAndOpts => friendAndOpts._1 -> friendAndOpts._2.drop((friendAndOpts._2.lastIndexWhere(optMsg => optMsg.isEmpty) + 1).max(0)))
        .map(friendAndOptsSliced => friendAndOptsSliced._1 -> friendAndOptsSliced._2.map(opt => opt.get)) //at this point all options should be present
      actorView.foreach(actor => actor ! ActorViewController.UpdateStoryMessage(toSend, recipient))
    case StringMessageFromConsole(message, recipient) =>
      //search map with key==recipient
      findInMap(recipient, userRefMap)
        .ifSuccess(foundRecipient => foundRecipient.head ! Message(message))
        .orElse(_ => {
          register ! GetServerRef(recipient)
          unstashAll()
          context.become({
            case ResponseForServerRefRequest(chatServer) => chatServer match {
              case Some(_) =>
                println("ChatServer found!")
                userRefMap += (recipient -> chatServer.get)
                unstashAll()
                context.unbecome()
                self ! StringMessageFromConsole(message, recipient)
              case _ =>
                println("ChatServer unreachable")
                unstashAll()
                context.unbecome()
            }
            case _ => stash()
          }, discardOld = false) // stack on top instead of replacing
        })
    case AttachmentMessageFromServer(attachment, senderName, messageNumber) =>
    //Display data on view/console
    case AttachmentMessageFromConsole(attachment, recipientName) =>
    //Same as TextMessageFromConsole
    case CreateGroupRequestFromConsole(groupName: String) =>
      register.tell(NewGroupChatRequest(groupName), self)
    case JoinGroupRequestFromConsole(groupName: String) =>
      register.tell(JoinGroupChatRequest(groupName), self)
    case ResponseForChatCreation(response) =>
      if (response) {
        println("Chat creation done!")
      } else println("Chat creation refused!")
    case LogInFromConsole(username) =>
      userName = username
      register ! JoinRequest(userName)
    case RequestForChatCreationFromConsole(friendName) =>
      users.find(user => user == friendName).fold({
        register ! AllUsersAndGroupsRequest
        unstashAll()
        context.become({
          case UserAndGroupActive(userList, groupList) =>
            users = userList
            groups = groupList
            unstashAll()
            context.unbecome()
            self ! RequestForChatCreationFromConsole(friendName)
          case _ => stash()
        }, discardOld = false) // stack on top instead of replacing
      })(user => register ! NewOneToOneChatRequest(user))
    case SetActorLogin(actorlogin) =>
      actorLogin = Option(actorlogin)
    case SetActorView(actorview) =>
      actorView = Option(actorview)
      actorView.get ! ActorViewController.UpdateUserAndGroupActive(users, groups)
    case StopRequest =>
      println("close request in client")
      register ! Unjoin()
      context.stop(self)
      context.system.terminate()
  }

}

object Client {

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
    * Get a message sent from server
    * @param message attachment sent
    * @param messageNumber the progressive number used to order all the exchanged messages
    */
  final case class StringMessageFromServer(message: String, messageNumber: Long, sender : String, recipient: String)

  /**
    *
    * @param message message from chat group
    * @param messageNumber message number
    * @param sender sender of message
    * @param group group name
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
    * @param payload the data that has been sent
    * @param messageNumber the progressive number of this message
    * @param sender the name of the sender
    * @param group the name of the group the sender (and the current user) belong to
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
    * @param actRef actor ref that manages one to one chat
    */
  final case class ResponseForServerRefRequest(actRef : Option[ActorRef])

  /**
    * Get username from LoginController
    * @param userName user name
    */
  final case class LogInFromConsole(userName:String)

  /**
    * Request to create a one to one chat from client console
    * @param friendName username to chat with
    */
  final case class RequestForChatCreationFromConsole(friendName : String)

  /**
    * Set reference for actor view
    * @param actorView ActorRef from View
    */
  final case class SetActorView(actorView : ActorRef)

  /**
    * Set reference for actor login
    * @param actorLogin ActorRef from Login
    */
  final case class SetActorLogin(actorLogin :ActorRef)

  final case class StopRequest()

}
