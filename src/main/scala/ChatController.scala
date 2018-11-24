import java.io.File
import java.net.URL
import java.nio.file.Paths
import java.util
import java.util.ResourceBundle

import ActorViewController._
import Client.{AttachmentMessageFromServer, StringMessageFromServer}
import Utils.interactionWithUI
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.ActionEvent
import javafx.fxml.Initializable
import javafx.geometry.Pos
import javafx.scene.control.Alert.AlertType
import javafx.scene.control._
import javafx.scene.image.{Image, ImageView}
import javafx.scene.input.{MouseButton, MouseEvent}
import javafx.scene.layout.{Background, BackgroundFill, HBox}
import javafx.scene.paint.Color
import javafx.scene.text.Text
import javafx.stage.FileChooser
import rumorsapp.{BubbleSpec, BubbledLabel}

class ChatController(userName : String, clientRef : ActorRef, system: ActorSystem) extends Initializable{

  import javafx.fxml.FXML

  @FXML
  var settingsImageView : ImageView = _
  @FXML
  var messageBox : TextArea = _
  @FXML
  var sendButton : Button = _
  @FXML
  var attachmentButton : Button = _
  @FXML
  var groupListView : ListView[HBox] = _
  @FXML
  var userListView : ListView[HBox] = _
  @FXML
  var usernameLabel : Label = _
  @FXML
  var chatPanel : ListView[HBox] = _
  @FXML
  var onlineCountLabel : Label = _
  @FXML
  var newChatGroupButton : Button = _

  var chatMessage: ObservableList[HBox] = FXCollections.observableArrayList()
  var storyMessageChat: Map[String,List[StringMessageFromServer]] = Map.empty
  var storyAttachmentChat: Map[String,List[AttachmentMessageFromServer]] = Map.empty
  var chatGroupFollowed: List[String] = List()
  var userList: List[String] = List()
  var groupList: List[String] = List()
  var actualUserSelected: String =  new String()
  var indexActualUserSelected : Int = _
  var listNotification : List[(String,Boolean)] = List()
  var fileContent : Option[Array[Byte]] = Option.empty


  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    system.actorOf(Props(classOf[ActorViewController],clientRef,this))
    usernameLabel.setText(userName)
  }

  @FXML
  def userSelected(): Unit = {
    if (userListView.getSelectionModel.getSelectedItem!=null) {
      actualUserSelected = userListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText
      indexActualUserSelected = userListView.getSelectionModel.getSelectedIndex
      clientRef ! Client.RequestForChatCreationFromConsole(actualUserSelected)
      //remove green notification
      updateUserGroupList(userList, groupList, None, Option(actualUserSelected))
      drawMessageView(actualUserSelected)
    }
  }

  @FXML
  def groupSelected(mouseEvent: MouseEvent): Unit = {
    if (mouseEvent.getButton.equals(MouseButton.SECONDARY)
      && !chatGroupFollowed.contains(groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText)) {
      println("dx click on " + groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText)
    } else groupListView.getSelectionModel.getSelectedItems.forEach(user => {
      val dialog = new Alert(AlertType.CONFIRMATION)
      dialog.setTitle("Confirmation Dialog")
      dialog.setHeaderText("Do you confirm to add to chatGroup: " + groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText)
      import javafx.scene.control.ButtonType
      val result = dialog.showAndWait
      if (result.get() == ButtonType.OK)
        clientRef ! Client.JoinGroupRequestFromConsole(groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText)
    })
  }

  @FXML
  def createChatGroupButton(event:ActionEvent) : Unit = {
    val dialog = new TextInputDialog()
    dialog.setTitle("Create new chat group")
    dialog.setHeaderText("Chat group name:")
    dialog.setContentText("Name:")
    val result = dialog.showAndWait
    result.ifPresent((chatGroupName: String) => clientRef ! Client.CreateGroupRequestFromConsole(chatGroupName))
  }

  def sendButtonAction(event:ActionEvent): Unit = {
    var itemNameSelected: Option[String] = Option.empty
    if (actualUserSelected!=null && actualUserSelected.length>0 && messageBox.getText().length>0) itemNameSelected = Option(actualUserSelected)
    else if (groupListView.getSelectionModel.getSelectedItem!=null)
      itemNameSelected = Option(groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText)
    itemNameSelected.foreach(item => clientRef ! Client.StringMessageFromConsole(messageBox.getText(), item))
    if (actualUserSelected!=null && actualUserSelected.length>0 && fileContent.isDefined) {
      println("Sending attachment")
      clientRef ! Client.AttachmentMessageFromConsole(fileContent.get, userName, actualUserSelected)
      attachmentButton.setStyle("-fx-base: lightgrey;")
      fileContent=Option.empty
    }
    messageBox.clear()
  }

  def attachmentButtonAction(event : ActionEvent) : Unit = {
    import java.nio.file.Files
    val fileChooser = new FileChooser
//    fileContent = Option.empty
    fileChooser.setTitle("Select an image to attach")
    val selectFile: Option[File] = Option(fileChooser.showOpenDialog(null))
    selectFile.foreach(selFile => {
      val img = new Image(selFile.toURI().toString())
      fileContent = Option(Files.readAllBytes(Paths.get(selFile.getAbsolutePath)))
      attachmentButton.setStyle("-fx-base: green;")
    })
  }

  //Draw useListView and groupListView
  def updateUserGroupList(users: List[String], groups: List[String], recipientNotification: Option[String], notificationToRemove: Option[String]): Unit = {
    userList.filterNot(users.contains(_)).foreach(notExistingUser => {
      storyMessageChat -= notExistingUser
      listNotification = listNotification.filterNot(_._1 == notExistingUser)
    })
    if (!users.contains(actualUserSelected)) {
      actualUserSelected = new String()
      userListView.getItems.clear()
    }
    userList = users
    groupList = groups
    users.foreach(elem => {
      recipientNotification.foreach(recipientNotif => if (elem == recipientNotif)
        listNotification = (elem, true) :: listNotification)
      notificationToRemove.foreach(notificationToRem => if (elem == notificationToRem)
        listNotification = listNotification.filter(_._1 != elem))
    })
    var convertoToObservable: util.ArrayList[HBox] = new util.ArrayList[HBox]()
    chatPanel.getItems.clear()
    users.filter(name => name != userName).foreach(name => {
      var hbox = new HBox()
      val texName = new Text(name)
      var imageView = new ImageView()
      texName.setStyle("-fx-font: 16 arial;")
      //add name to list if is not contained in listNotification
      if (listNotification.contains(name, true)) {
        val onLineImage = new Image(getClass.getClassLoader.getResource("res/img/online.png").toString)
        imageView = new ImageView(onLineImage)
        hbox.getChildren.addAll(texName, imageView) //textName and image
      } else hbox.getChildren.addAll(texName, imageView) //textName and image
      hbox.setAlignment(Pos.CENTER_LEFT)
      convertoToObservable.add(hbox)
      val userListHbox: ObservableList[HBox] = FXCollections.observableArrayList(convertoToObservable)
      userListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      userListView.setItems(userListHbox)
      //focus element on userList if a user is selected
      if (actualUserSelected.length>0) userListView.getSelectionModel.selectIndices(indexActualUserSelected)
      //set label with users count
      onlineCountLabel.setText(userListHbox.size().toString)
    })
    //groupList
    convertoToObservable.clear()
    groups.foreach(elem => {
      var hbox = new HBox()
      val textName = new Text(elem)
      textName.setStyle("-fx-font: 16 arial; -fx-fill: black;")
      if (chatGroupFollowed.contains(elem)) textName.setStyle("-fx-fill: green")
      hbox.getChildren.addAll(textName) //textName and image
      hbox.setAlignment(Pos.CENTER_LEFT)
      convertoToObservable.add(hbox)
    })
    val groupListHbox: ObservableList[HBox] = FXCollections.observableArrayList(convertoToObservable)
    groupListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
    groupListView.setItems(groupListHbox)
  }

  def updateMessageView(recipient : String) : Unit = {
    //if message comes from a different chat among the one selected, there are more than 1 chat and sender is not recipient
    if (recipient != actualUserSelected ) {
      //redraw userList with green notification for recipient
      updateUserGroupList(userList, groupList, Option(recipient),Option.empty)
    } else drawMessageView(recipient)
  }

  def drawMessageView(recipient: String): Unit = {
    import scala.math.max
    chatPanel.getItems.clear()
    var numMsgItems = 0
    var numAttachItems = 0
    if (storyMessageChat.contains(recipient)) numMsgItems = storyMessageChat(recipient).size
    if (storyAttachmentChat.contains(recipient)) numAttachItems = storyAttachmentChat(recipient).size
    val numItems = max(numMsgItems, numAttachItems)
    if (numItems > 0) {
      def mergeList(counter: Int, mergedList: Map[String, List[ComboMessage]], storyMessageChat: Map[String, List[StringMessageFromServer]], storyAttachmentChat: Map[String, List[AttachmentMessageFromServer]]): Map[String, List[ComboMessage]] = counter match {
        case a: Int if a <= numItems =>
          //get message with sequence==conterr
          var message = storyMessageChat.filter(elem => elem._1 == recipient).map(elem => elem._2.filter(elem => elem.messageNumber == counter)).filter(elem => elem.nonEmpty).map(elem => elem.head).headOption
          var attach = storyAttachmentChat.filter(elem => elem._1 == recipient).map(elem => elem._2.filter(elem => elem.messageNumber == counter)).filter(elem => elem.nonEmpty).map(elem => elem.head).headOption
          var tempList: List[ComboMessage] = {
            if (mergedList.nonEmpty) ComboMessage(message, attach) :: mergedList(recipient)
            else List(ComboMessage(message, attach))
          }
          mergeList(counter + 1, Map(recipient -> tempList), storyMessageChat, storyAttachmentChat)
        case _ => mergedList
      }
      val allMessageForRecipient: Map[String, List[ComboMessage]] = mergeList(1, Map.empty, storyMessageChat, storyAttachmentChat)
      if (allMessageForRecipient.contains(recipient)) {
        val comboMessageForRecipient = allMessageForRecipient(recipient)
        comboMessageForRecipient.map(comboMsg => {
          if ((comboMsg.stringMessage.isDefined && (comboMsg.stringMessage.get.sender == userName)) ||
            (comboMsg.attachemtMessage.isDefined && (comboMsg.attachemtMessage.get.sender == userName))) {
            val stringMsg = comboMsg.stringMessage
            val attachMsg = comboMsg.attachemtMessage
            val bubble: BubbledLabel = new BubbledLabel
            val horizontalBox = new HBox
            if (stringMsg.isDefined) {
              bubble.setText(stringMsg.get.message)
            } else {
              import java.io.ByteArrayInputStream
              println("found image")
              val img = new Image(new ByteArrayInputStream(attachMsg.get.payload))
              bubble.setGraphic(new ImageView(img))
            }
            horizontalBox.setMaxWidth(chatPanel.getWidth - 20)
            horizontalBox.setAlignment(Pos.TOP_RIGHT)
            bubble.setBubbleSpec(BubbleSpec.FACE_RIGHT_CENTER)
            horizontalBox.getChildren.addAll(bubble)
            bubble.setBackground(new Background(new BackgroundFill(Color.LIGHTGREEN, null, null)))

            horizontalBox
          } else {
            val stringMsg = comboMsg.stringMessage
            val attachMsg = comboMsg.attachemtMessage
            val bubble: BubbledLabel = new BubbledLabel
            if (stringMsg.isDefined) bubble.setText(stringMsg.get.message)
            bubble.setBackground(new Background(new BackgroundFill(Color.AQUAMARINE, null, null)))
            val horizontalBox = new HBox
            bubble.setBubbleSpec(BubbleSpec.FACE_LEFT_CENTER)
            horizontalBox.getChildren.addAll(bubble)
            horizontalBox
          }
        }).foreach(hbox => chatPanel.getItems.add(hbox))
      }
    }
  }

  def updateMessageStory(storyMessage: Map[String,List[StringMessageFromServer]]) : Unit = storyMessageChat = storyMessage

  def updateAttachMentStory(storyAttachment: Map[String,List[AttachmentMessageFromServer]]) : Unit = storyAttachmentChat = storyAttachment

  def addOwnerToGroupAfterCreation(response: Boolean): Unit = if (response) clientRef ! Client.JoinGroupRequestFromConsole(userName)

  def addChatGroup(response: Boolean, groupName: String) : Unit = if (response) groupName::chatGroupFollowed

  def removeChatGroup(response: Boolean, groupName: String) : Unit = if (response) chatGroupFollowed = chatGroupFollowed.filter(_ != groupName)

}

case class ComboMessage(stringMessage: Option[StringMessageFromServer],attachemtMessage: Option[AttachmentMessageFromServer])

class ActorViewController(clientRef : ActorRef, chatController : ChatController) extends Actor {

  override def preStart(): Unit = {
    clientRef ! Client.SetActorView(self)
  }

  override def receive: Receive = {
    case UpdateUserAndGroupActive(userList:List[String], groupList:List[String])=>
      interactionWithUI {
        chatController.updateUserGroupList(userList, groupList, Option.empty, Option.empty)
      }
    case ResponseForChatCreation(_: Boolean) => Unit
    case ResponseForChatGroupCreation(response : Boolean) =>
      interactionWithUI {
        chatController.addOwnerToGroupAfterCreation(response: Boolean)
      }
    case UpdateStoryMessage(storyMessage : Map[String,List[StringMessageFromServer]],recipient : String) =>
      interactionWithUI {
        chatController.updateMessageStory(storyMessage)
        chatController.updateMessageView(recipient)
      }
    case UpdateStoryAttachment(storyAttachment: Map[String,List[AttachmentMessageFromServer]],recipient : String) =>
      interactionWithUI{
        chatController.updateAttachMentStory(storyAttachment)
        chatController.updateMessageView(recipient)
      }
    case ResponseForJoinGroupToConsole(response: Boolean, groupName: String) =>
      interactionWithUI {
        chatController.addChatGroup(response, groupName)
      }
    case ResponseForUnJoinGroupToConsole(response: Boolean, groupName: String) =>
      interactionWithUI {
        chatController.removeChatGroup(response, groupName)
      }
  }
}

object ActorViewController {

  /**
    * Get all users and chat group active
    * @param userList  list of username
    * @param groupList list of chat group
    */
  final case class UpdateUserAndGroupActive(userList: List[String], groupList: List[String])

  /**
    * Response about request to create a one to one chat
    * @param accept
    */
  final case class ResponseForChatCreation(accept: Boolean)

  /**
    * Response about request to create a chat group
    * @param accept
    */
  final case class ResponseForChatGroupCreation(accept: Boolean)

  /**
    * Receives all messages received
    * @param storyMessage map which stores for every recipient (key) all messages received
    */
  final case class UpdateStoryMessage(storyMessage: Map[String, List[StringMessageFromServer]], recipient: String)

  /**
    * Response about request to join to a chat group
    * @param response
    */
  final case class ResponseForJoinGroupToConsole(response: Boolean, groupName: String)

  /**
    * Response about request to unjoin from a chat group
    * @param response
    */
  final case class ResponseForUnJoinGroupToConsole(response: Boolean, groupName: String)

  final case class UpdateStoryAttachment(storyAttachment: Map[String, List[AttachmentMessageFromServer]], recipient: String)
}