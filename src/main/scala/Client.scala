import java.io.File

import ActorLoginController.ResponseFromLogin
import Client._
import OneToOneChatServer.{Attachment, Message}
import RegisterServer._
import akka.actor.{Actor, ActorRef, ActorSelection, ExtendedActorSystem, Stash}
import com.typesafe.config.ConfigFactory
import Utils._
import akka.actor.TypedActor.PostStop

class Client(system: ExtendedActorSystem) extends Actor with Stash with PostStop {

  var users: List[String] = List()
  var groups: List[String] = List()
  var userRefMap: Map[String, ActorRef] = Map.empty
  var groupRefMap: Map[String, ActorRef] = Map.empty
  val registerFilePath: String = "src/main/scala/res/server.conf"
  var userName: String = _
  var storyMessageChat: Map[String, List[Option[StringMessageFromServer]]] = Map.empty //key is recipient
  var storyAttachmentChat: Map[String,List[AttachmentMessageFromServer]] = Map.empty
  var storyComboChat: Map[String, List[ComboMessage]] = Map.empty
  var storyComboGroupChat: Map[String, List[ComboGroupMessage]] = Map.empty

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
      findInMap(recipient, storyComboChat)
        .ifSuccess(existingRecip  => {
          var tmpHistoryComboMsg : List[ComboMessage] = existingRecip.head
          tmpHistoryComboMsg = ComboMessage(Option(StringMessageFromServer(message,messageNumber,senderName,recipientName)),Option.empty) :: tmpHistoryComboMsg
          storyComboChat += (recipient -> tmpHistoryComboMsg)
        })
        .orElse(_ => storyComboChat += (recipient -> List(ComboMessage(Option(StringMessageFromServer(message,messageNumber,senderName,recipientName)),Option.empty))))
      val newComboMsg = ComboMessage(Option(StringMessageFromServer(message,messageNumber,senderName,recipientName)),Option.empty)
//      checkOrderForOneToOneChat(newComboMsg,messageNumber,senderName, recipientName)
      actorView.foreach(actor => actor ! ActorViewController.UpdateStoryComboMessage(storyComboChat,senderName))
//      actorView.foreach(actor => actor ! ActorViewController.UpdateStoryAttachment(storyAttachmentChat, recipient))
//      findInMap(recipient, storyMessageChat).ifSuccess(messagesList => {
//        var temp = messagesList.head.toArray
//        if (messageNumber > temp.head.get.messageNumber) {
//          for (_ <- temp.head.get.messageNumber + 1 until messageNumber) {
//            temp = Option.empty +: temp
//          }
//          temp = Option(StringMessageFromServer(message, messageNumber, senderName, recipient)) +: temp
//        } else {
//          temp(temp.length - messageNumber.toInt) = Option(StringMessageFromServer(message, messageNumber, senderName, recipient))
//        }
//        storyMessageChat += (recipient -> temp.toList)
//      }).orElse(_ => storyMessageChat += (recipient -> List(Option(StringMessageFromServer(message, messageNumber, senderName, recipient)))))
//      val toSend = storyMessageChat
//        .map(friendAndOpts => friendAndOpts._1 -> friendAndOpts._2.drop((friendAndOpts._2.lastIndexWhere(optMsg => optMsg.isEmpty) + 1).max(0)))
//        .map(friendAndOptsSliced => friendAndOptsSliced._1 -> friendAndOptsSliced._2.map(opt => opt.get)) //at this point all options should be present
//      actorView.foreach(actor => actor ! ActorViewController.UpdateStoryComboMessage(toSend, recipient))
    case AttachmentMessageFromServer(attachment, messageNumber,senderName, recipientName) =>
      val recipient = if (userName != senderName) senderName else recipientName
      findInMap(recipient, storyComboChat)
        .ifSuccess(existingRecip  => {
          var tmpHistoryComboMsg : List[ComboMessage] = existingRecip.head
          tmpHistoryComboMsg = ComboMessage(Option.empty,Option(AttachmentMessageFromServer(attachment,messageNumber,senderName,recipientName))) :: tmpHistoryComboMsg
          storyComboChat += (recipient -> tmpHistoryComboMsg)
        })
        .orElse(_ => storyComboChat += (recipient -> List(ComboMessage(Option.empty,Option(AttachmentMessageFromServer(attachment,messageNumber,senderName,recipientName))))))
      val newComboMsg = ComboMessage(Option.empty,Option(AttachmentMessageFromServer(attachment,messageNumber,senderName,recipientName)))
//      checkOrderForOneToOneChat(newComboMsg,messageNumber,senderName, recipientName)
      actorView.foreach(actor => actor ! ActorViewController.UpdateStoryComboMessage(storyComboChat, recipient))
    case StringMessageFromConsole(message, recipient) =>
      //search map with key==recipient
      findInMap(recipient, userRefMap)
        .ifSuccess(foundRecipient => foundRecipient.head ! Message(message))
        .orElse(_ => searchRefFromRegister(recipient, ()=> self ! StringMessageFromConsole(message, recipient)))
    case AttachmentMessageFromServer(attachment, messageNumber,senderName, recipientName) => Unit
    case AttachmentMessageFromConsole(attachment, senderName, recipientName) =>
      findInMap(recipientName, userRefMap)
        .ifSuccess(foundRecipient => foundRecipient.head ! Attachment(attachment, senderName, recipientName))
        .orElse(_ => searchRefFromRegister(recipientName, ()=> self ! AttachmentMessageFromConsole(attachment, senderName, recipientName)))
    case CreateGroupRequestFromConsole(groupName: String) =>
      register.tell(NewGroupChatRequest(groupName), self)
    case JoinGroupRequestFromConsole(groupName: String) =>
      findInMap(groupName, userRefMap)
        .ifSuccess(foundRecipient => foundRecipient.head ! GroupChatServer.AddMember(userName,self))
        .orElse(_ => searchRefFromRegister(groupName, ()=> self ! JoinGroupRequestFromConsole(groupName: String)))
    case UnJoinGroupRequestFromConsole(groupName: String) =>
      findInMap(groupName, userRefMap)
        .ifSuccess(foundRecipient => foundRecipient.head ! GroupChatServer.RemoveMember(userName))
    case ResponseForChatCreation(response) =>
      if (response) {
        println("Chat creation done!")
        actorView.foreach(actor => actor ! ActorViewController.ResponseForChatCreation(response))
      } else println("Chat creation refused!")
    case ResponseForJoinGroupRequest(response : Boolean, groupName : String) =>
      if (response) {
        println("Joining to "+ groupName + " accepted!")
        actorView.foreach(actor => actor ! ActorViewController.ResponseForJoinGroupToConsole(response,groupName))
      } else println("Joined to "+ groupName + " refused!")
    case ResponseForUnJoinGroupRequest(response : Boolean, groupName : String) =>
      if (response) {
        println("Unjoinig from "+ groupName + " accepted!")
        actorView.foreach(actor => actor ! ActorViewController.ResponseForUnJoinGroupToConsole(response,groupName))
      } else println("UnJoined from "+ groupName + " refused!")
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

  def searchRefFromRegister(nameToSearch: String, recursiveMessage: () => Unit): Unit = {
    register ! GetServerRef(nameToSearch : String)
    context.become({
      case ResponseForServerRefRequest(chatGroupServer) => chatGroupServer match {
        case Some(_) =>
          println("ChatServer/ChatGroupServer found!")
          userRefMap += (nameToSearch -> chatGroupServer.get)
          unstashAll()
          context.unbecome()
          recursiveMessage()
        case _ =>
          println("ChatServer/ChatGroupServer unreachable")
          unstashAll()
          context.unbecome()
      }
      case _ => stash()
    }, discardOld = false) // stack on top instead of replacing
  }

  def checkOrderForOneToOneChat(comboMsg: ComboMessage,messageNumber: Long,senderName: String, recipientName: String) : Unit = {
  }
}

case class ComboMessage(stringMessage: Option[StringMessageFromServer],attachmetMessage: Option[AttachmentMessageFromServer])

case class ComboGroupMessage(stringMessage: Option[StringMessageFromGroupServer],attachmetMessage: Option[AttachmentMessageFromGroupServer])

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
  final case class AttachmentMessageFromServer(payload : Array[Byte], messageNumber: Long, sender : String, recipient: String)

  /**
    * Get an attachment sent from client console
    * @param payload attachment sent
    * @param sender name of sender
    * @param recipient name of recpient
    */
  final case class AttachmentMessageFromConsole(payload : Array[Byte], sender: String, recipient : String)

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
    * Request to join to chat a group from client console
    * @param groupName name of the group
    */
  final case class UnJoinGroupRequestFromConsole(groupName:String)

  /**
    * Response from server about client request for chat creation
    * @param accept response from server
    */
  final case class ResponseForChatCreation(accept : Boolean)

  /**
    * Response from server about client request to join to an existing chat group
    * @param accept response from server
    * @param groupName group name
    */
  final case class ResponseForJoinGroupRequest(accept : Boolean, groupName: String)

  /**
    * Response from server about client request to join to an existing chat group
    * @param accept response from server
    * @param groupName group name
    */
  final case class ResponseForUnJoinGroupRequest(accept : Boolean, groupName: String)

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

  /**
    * Request to close Client activity
    */
  final case class StopRequest()

}
