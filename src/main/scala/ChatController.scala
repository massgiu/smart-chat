import java.io.{ByteArrayInputStream, File}
import java.net.URL
import java.nio.file.{Files, Paths}
import java.util
import java.util.ResourceBundle

import Client.{AttachmentMessageFromServer, StringMessageFromGroupServer, StringMessageFromServer}
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
import ActorViewController._

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

  private val onlineImagePath = getClass.getClassLoader.getResource("res/img/online.png").toString

  var chatMessage: ObservableList[HBox] = FXCollections.observableArrayList()
  var storyComboMessageChat: Map[String,List[ComboMessage]] = Map.empty
  var storyComboGroupMessageChat: Map[String,List[ComboMessage]] = Map.empty
  var chatGroupFollowed: List[String] = List()
  var userList: List[String] = List()
  var groupList: List[String] = List()
  var isGroupSelected: Boolean = false
  var actualUserSelected: String =  new String()
  var indexActualUserSelected : Int = _
  var listNotification: List[String] = List.empty
  var groupListNotification: List[String] = List.empty
  var fileContent : Option[Array[Byte]] = Option.empty


  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    system.actorOf(Props(classOf[ActorViewController],clientRef,this))
    usernameLabel.setText(userName)
  }

  @FXML
  def userSelected(): Unit = {
    if (userListView.getSelectionModel.getSelectedItem != null) {
      isGroupSelected = false
      actualUserSelected = userListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText
      indexActualUserSelected = userListView.getSelectionModel.getSelectedIndex
      clientRef ! Client.RequestForChatCreationFromConsole(actualUserSelected)
      //remove green notification
      updateUserGroupList(userList, groupList, None, Option((actualUserSelected, false)))
      drawMessageView(actualUserSelected, isGroup = false)
    }
  }

  @FXML
  def groupSelected(mouseEvent: MouseEvent): Unit = {
    if (groupListView.getSelectionModel.getSelectedItem != null) {
      val selectedGroupName = groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText
      val selectedGroupIndex = groupListView.getSelectionModel.getSelectedIndex
      if (mouseEvent.getButton.equals(MouseButton.PRIMARY)) {
        if (!chatGroupFollowed.contains(selectedGroupName)) {
          val dialog = new Alert(AlertType.CONFIRMATION)
          dialog.setTitle("Confirmation Dialog")
          dialog.setHeaderText("Do you want to subscribe to " + selectedGroupName + "?")
          val result = dialog.showAndWait
          if (result.get() == ButtonType.OK) {
            clientRef ! Client.JoinGroupRequestFromConsole(selectedGroupName)
          }
        }
        isGroupSelected = true
        actualUserSelected = selectedGroupName
        indexActualUserSelected = selectedGroupIndex
        //remove green notification
        updateUserGroupList(userList, groupList, None, Option((actualUserSelected, true)))
        drawMessageView(actualUserSelected, isGroup = true)
      } else if (mouseEvent.getButton.equals(MouseButton.SECONDARY) && !chatGroupFollowed.contains(selectedGroupName)) {
        println("dx click on " + selectedGroupName)
      }
    }
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
    var itemNameSelected: Option[String] = Option(actualUserSelected)
    //sending Message
    if (messageBox.getText().length>0) {
      itemNameSelected.foreach(item => clientRef ! Client.StringMessageFromConsole(messageBox.getText(), item, isGroupSelected))
    } else if (fileContent.isDefined){
      itemNameSelected.foreach(recipient => clientRef ! Client.AttachmentMessageFromConsole(fileContent.get, userName, recipient, isGroupSelected))
      attachmentButton.setStyle("-fx-base: lightgrey;")
      fileContent = Option.empty
    }
    messageBox.clear()
  }

  def attachmentButtonAction(event : ActionEvent) : Unit = {
    val fileChooser = new FileChooser
    fileChooser.setTitle("Select an image to attach")
    val selectFile: Option[File] = Option(fileChooser.showOpenDialog(null))
    selectFile.foreach(selFile => {
      val img = new Image(selFile.toURI().toString())
      fileContent = Option(Files.readAllBytes(Paths.get(selFile.getAbsolutePath)))
      attachmentButton.setStyle("-fx-base: green;")
    })
  }

  //Draw useListView and groupListView
  def updateUserGroupList(users: List[String], groups: List[String], recipientNotification: Option[(String, Boolean)], notificationToRemove: Option[(String, Boolean)]): Unit = {
    //data structures update
    userList = users
    groupList = groups
    userList.filterNot(users.contains(_)).foreach(notExistingUser => {
      storyComboMessageChat -= notExistingUser
      listNotification = listNotification.filterNot(_ == notExistingUser)
    })
    if (!isGroupSelected && !users.contains(actualUserSelected)) {
      actualUserSelected = new String()
      chatPanel.getItems.clear()
    }
    groupList.filterNot(groups.contains(_)).foreach(notExistingGroup => {
      storyComboGroupMessageChat -= notExistingGroup
      chatGroupFollowed = chatGroupFollowed.filterNot(_ == notExistingGroup)
      groupListNotification = groupListNotification.filterNot(_ == notExistingGroup)
    })
    if (isGroupSelected && !groups.contains(actualUserSelected)) {
      actualUserSelected = new String()
      chatPanel.getItems.clear()
    }
    userListView.getItems.clear()
    groupListView.getItems.clear()
    recipientNotification.foreach(from => {
      if (from._2) {
        groups.find(_ == from._1).foreach(_ => groupListNotification = from._1 :: groupListNotification)
      } else {
        users.find(_ == from._1).foreach(_ => listNotification = from._1 :: listNotification)
      }
    })
    notificationToRemove.foreach(from => {
      if (from._2) {
        groups.find(_ == from._1).foreach(_ => groupListNotification = groupListNotification.filter(_ != from._1))
      } else {
        users.find(_ == from._1).foreach(_ => listNotification = listNotification.filter(_ != from._1))
      }
    })

    //users list graphical update
    val usersGraphicalList: util.ArrayList[HBox] = new util.ArrayList[HBox]()
    users.filter(name => name != userName).foreach(name => {
      val userAndIconContainer = new HBox()
      val textName = new Text(name)
      var icon = new ImageView()
      textName.setStyle("-fx-font: 16 arial;")
      if (listNotification.contains(name)) {
        val onLineImage = new Image(onlineImagePath)
        icon = new ImageView(onLineImage)
        userAndIconContainer.getChildren.addAll(textName, icon)
      } else userAndIconContainer.getChildren.addAll(textName, icon)
      userAndIconContainer.setAlignment(Pos.CENTER_LEFT)
      usersGraphicalList.add(userAndIconContainer)
      val userListHBox: ObservableList[HBox] = FXCollections.observableArrayList(usersGraphicalList)
      userListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      userListView.setItems(userListHBox)
      if (!isGroupSelected && actualUserSelected.length>0) userListView.getSelectionModel.selectIndices(indexActualUserSelected)
    })
    onlineCountLabel.setText(userListView.getItems.size().toString)

    //groups list graphical update
    val groupsGraphicalList: util.ArrayList[HBox] = new util.ArrayList[HBox]()
    groups.foreach(group => {
      val groupAndIconContainer = new HBox()
      val textName = new Text(group)
      var icon = new ImageView()
      textName.setStyle("-fx-font: 16 arial;")
      if (chatGroupFollowed.contains(group)) textName.setFill(Color.GREEN)
      if (groupListNotification.contains(group)) {
        val onLineImage = new Image(onlineImagePath)
        icon = new ImageView(onLineImage)
        groupAndIconContainer.getChildren.addAll(textName, icon)
      } else groupAndIconContainer.getChildren.addAll(textName, icon)
      groupAndIconContainer.setAlignment(Pos.CENTER_LEFT)
      groupsGraphicalList.add(groupAndIconContainer)
      val groupListHBox: ObservableList[HBox] = FXCollections.observableArrayList(groupsGraphicalList)
      groupListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      groupListView.setItems(groupListHBox)
      if (isGroupSelected && actualUserSelected.length>0) groupListView.getSelectionModel.selectIndices(indexActualUserSelected)
    })
  }

  def updateMessageView(recipient: String, isGroup: Boolean) : Unit = {
    //if message comes from a different chat among the one selected, there are more than 1 chat and sender is not recipient
    if (isGroupSelected == isGroup && recipient == actualUserSelected) {
      drawMessageView(recipient, isGroup)
    } else {
      if (!isGroup) {
        updateUserGroupList(userList, groupList, Option((recipient, false)),Option.empty)
      } else {
        updateUserGroupList(userList, groupList, Option((recipient, true)),Option.empty)
      }
    }
  }

  def drawMessageView(recipient: String, isGroup: Boolean): Unit = {
    chatPanel.getItems.clear()
    val source = if (isGroup) storyComboGroupMessageChat else storyComboMessageChat
    if (source.contains(recipient)) {
      val comboMessageForRecipient = source(recipient)
      comboMessageForRecipient.map(comboMsg => {
        val bubble: BubbledLabel = new BubbledLabel
        val horizontalBox = new HBox
        val stringMsg = comboMsg.stringMessage
        val attachMsg = comboMsg.attachmetMessage
        if (stringMsg.isDefined) {
          bubble.setText(stringMsg.get.sender + ": " + stringMsg.get.message)
        } else if (attachMsg.isDefined){
          val imageView = new ImageView(new Image(new ByteArrayInputStream(attachMsg.get.payload)))
          imageView.setFitHeight(150)
          imageView.setPreserveRatio(true)
          bubble.setGraphic(imageView)
        }
        if ((comboMsg.stringMessage.isDefined && (comboMsg.stringMessage.get.sender == userName)) ||
          (comboMsg.attachmetMessage.isDefined && (comboMsg.attachmetMessage.get.sender == userName))) {

          horizontalBox.setMaxWidth(chatPanel.getWidth - 20)
          horizontalBox.setAlignment(Pos.TOP_RIGHT)
          bubble.setBubbleSpec(BubbleSpec.FACE_RIGHT_CENTER)
          horizontalBox.getChildren.addAll(bubble)
          bubble.setBackground(new Background(new BackgroundFill(Color.LIGHTGREEN, null, null)))
          horizontalBox
        } else {
          bubble.setBackground(new Background(new BackgroundFill(Color.AQUAMARINE, null, null)))
          bubble.setBubbleSpec(BubbleSpec.FACE_LEFT_CENTER)
          horizontalBox.getChildren.addAll(bubble)
          horizontalBox
        }
      }).foreach(hbox => chatPanel.getItems.add(hbox))
    }
  }

  def updateComboMessageStory(storyComboMessage: Map[String,List[ComboMessage]]) : Unit = storyComboMessageChat = storyComboMessage

  def updateComboGroupMessageStory(storyComboGroupMessage: Map[String,List[ComboGroupMessage]]) : Unit = {
    storyComboGroupMessageChat = storyComboGroupMessage
      .map(groupMsgs => groupMsgs._1 -> groupMsgs._2
        .map(msg => ComboMessage(msg.stringGroupMessage.map(msg => StringMessageFromServer(msg.message, msg.messageNumber, msg.sender, msg.group)),
          msg.attachmetGroupMessage.map(msg => AttachmentMessageFromServer(msg.payload, msg.messageNumber, msg.sender, msg.group)))))
  }

  def addOwnerToGroupAfterCreation(response: Boolean): Unit = if (response) clientRef ! Client.JoinGroupRequestFromConsole(userName)

  def addChatGroup(response: Boolean, groupName: String) : Unit = if (response) {
    chatGroupFollowed = groupName::chatGroupFollowed
    updateUserGroupList(userList, groupList, Option.empty, Option.empty)
  }

  def removeChatGroup(response: Boolean, groupName: String) : Unit = if (response) chatGroupFollowed = chatGroupFollowed.filter(_ != groupName)

}

case class NewMessage(message: String, messageIndex: Long, originalSender: String, originalRecipient: String)

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
    case UpdateStoryComboMessage(storyComboMessage : Map[String,List[ComboMessage]],recipient : String) =>
      interactionWithUI {
        chatController.updateComboMessageStory(storyComboMessage)
        chatController.updateMessageView(recipient, isGroup = false)
      }
    case UpdateStoryComboGroupMessage(storyComboGroupMessage : Map[String,List[ComboGroupMessage]],groupName : String) =>
      interactionWithUI {
        chatController.updateComboGroupMessageStory(storyComboGroupMessage)
        chatController.updateMessageView(groupName, isGroup = true)
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
    * Response about request to join to a chat group
    * @param response
    */
  final case class ResponseForJoinGroupToConsole(response: Boolean, groupName: String)

  /**
    * Response about request to unjoin from a chat group
    * @param response
    */
  final case class ResponseForUnJoinGroupToConsole(response: Boolean, groupName: String)

  /**
    * @param storyMessage map which stores for every single chat (friend name is key) all messages received
    * @param recipient recipient of combo messages
    */
  final case class UpdateStoryComboMessage(storyMessage: Map[String, List[ComboMessage]], recipient: String)

  /**
    * @param storyGroupMessage map which stores for every group joined (key) all messages received
    * @param recipient recipient of combo messages
    */
  final case class UpdateStoryComboGroupMessage(storyGroupMessage: Map[String, List[ComboGroupMessage]], recipient: String)

}