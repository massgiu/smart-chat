
import java.io.File
import java.net.URL
import java.util
import java.util.ResourceBundle

import ActorViewController.{ResponseForChatCreation, UpdateStoryMessage, UpdateUserAndGroupActive}
import Client.StringMessageFromServer
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import javafx.application.{Application, Platform}
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.ActionEvent
import javafx.fxml.Initializable
import javafx.geometry.Pos
import javafx.scene.control.Alert.AlertType
import javafx.scene.control._
import javafx.scene.image.{Image, ImageView}
import javafx.stage.{FileChooser, Stage}
import javafx.scene.layout.Background
import javafx.scene.layout.BackgroundFill
import javafx.scene.layout.HBox
import javafx.scene.paint.Color
import javafx.scene.text.Text
import rumorsapp.{BubbleSpec, BubbledLabel}

class LaunchClientView extends Application{
  override def start(primaryStage: Stage): Unit = {
    ActorSystem.create("MySystem",ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
  }
}

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

  var chatMessage : ObservableList[HBox] = FXCollections.observableArrayList()
  var storyMessageChat : Map[String,List[StringMessageFromServer]] = Map.empty
  var userList : List[String] = List()
  var groupList : List[String] = List()
  var actualUserSelected: String =  new String()
  var indexActualUserSelected : Int = _
  var listNotification : List[(String,Boolean)] = List()


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
  def groupSelected(): Unit = {
    groupListView.getSelectionModel.getSelectedItems.forEach(user => {
      val dialog = new Alert(AlertType.CONFIRMATION)
      dialog.setTitle("Confirmation Dialog")
      dialog.setHeaderText("Do you confirm to add to chatGroup: " + groupListView.getSelectionModel.getSelectedItem)
      import javafx.scene.control.ButtonType
      val result = dialog.showAndWait
      if (result.get() == ButtonType.OK) println("Request to add to chatGroup: " + groupListView.getSelectionModel.getSelectedItem)
    })
  }

  @FXML
  def createChatGroupButton(event:ActionEvent) : Unit = {
    val dialog = new TextInputDialog()
    dialog.setTitle("Create new chat group")
    dialog.setHeaderText("Chat group name:")
    dialog.setContentText("Name:")
    val result = dialog.showAndWait
    result.ifPresent((chatGroupName: String) => println("Request to create chatGroup: " + chatGroupName))
  }

  def sendButtonAction(event:ActionEvent): Unit = {
    var itemNameSelected = new String
    if (actualUserSelected!=null) itemNameSelected = actualUserSelected
    else if (groupListView.getSelectionModel.getSelectedItem!=null)
      itemNameSelected = groupListView.getSelectionModel.getSelectedItem.getChildren.get(0).asInstanceOf[Text].getText
    clientRef ! Client.StringMessageFromConsole(messageBox.getText(), itemNameSelected)
    messageBox.clear()
  }

  def attachmentButtonAction(event : ActionEvent) : Unit = {
    val fileChooser = new FileChooser
    val selectFile = fileChooser.showOpenDialog(null)
    if (selectFile != null) {
      val filePath =  selectFile.getAbsolutePath
      println(filePath)
    } else println("File is not valid")
  }

  //Draw useListView and groupListView
  def updateUserGroupList(users:List[String], groups:List[String], recipientNotification : Option[String], notificationToRemove : Option[String]) : Unit ={
    userList = users
    groupList = groups
    users.foreach(elem => {
      recipientNotification.foreach(recipientNotif => if (elem==recipientNotif)
        listNotification = (elem, true)::listNotification)
      notificationToRemove.foreach(notificationToRem=> if (elem==notificationToRem)
        listNotification = listNotification.filter(_._1!=elem))
    })
    Platform.runLater(()=>  {
      var convertoToObservable : util.ArrayList[HBox] = new util.ArrayList[HBox]()
      users.filter(name=>name!=userName).foreach(name=>{
        var hbox = new HBox()
        val texName = new Text(name)
        var imageView  = new ImageView()
        texName.setStyle("-fx-font: 16 arial;")
        //add name to list if is not contained in listNotification
        if (listNotification.contains(name,true)){
          val onLineImage = new Image(getClass.getClassLoader.getResource("res/img/online.png").toString)
          imageView = new ImageView(onLineImage)
          hbox.getChildren.addAll(texName,imageView) //textName and image
        } else hbox.getChildren.addAll(texName,imageView) //textName and image

        hbox.setAlignment(Pos.CENTER_LEFT)
        convertoToObservable.add(hbox)
        val userList: ObservableList[HBox] = FXCollections.observableArrayList(convertoToObservable)
        userListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
        userListView.setItems(userList)
        //focus element on userList if a user is selected
        if (actualUserSelected!="") userListView.getSelectionModel.selectIndices(indexActualUserSelected)
        //set label with users count
        onlineCountLabel.setText(userList.size().toString)
      })

      //groupList
      convertoToObservable.clear()
      groups.foreach(elem=>{
        var hbox = new HBox()
        val texName = new Text(elem)
        texName.setStyle("-fx-font: 16 arial;")
        hbox.getChildren.addAll(texName) //textName and image
        hbox.setAlignment(Pos.CENTER_LEFT)
        convertoToObservable.add(hbox)
      })
      var groupList: ObservableList[HBox] = FXCollections.observableArrayList(convertoToObservable)
      groupListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      groupListView.setItems(groupList)
    })
  }

  def updateMessageView(recipient : String) : Unit = {
    //if message comes from a different chat among the one selected, there are more than 1 chat and sender is not recipient
    if (userListView.getItems.size() > 1 && recipient != actualUserSelected ) {
      //redraw userList with green notification for recipient
      updateUserGroupList(userList, groupList, Option(recipient),None)
    } else drawMessageView(recipient)
  }

  def drawMessageView(recipient : String): Unit ={
    Platform.runLater(()=> {
      chatPanel.getItems.clear()
      if (storyMessageChat.contains(recipient)) {
        val allMessageForRecipient = storyMessageChat(recipient)
        allMessageForRecipient.map(msg => if (msg.sender == userName) {
          val bubble: BubbledLabel = new BubbledLabel
          bubble.setText(msg.message)
          bubble.setBackground(new Background(new BackgroundFill(Color.LIGHTGREEN, null, null)))
          val horizontalBox = new HBox
          horizontalBox.setMaxWidth(chatPanel.getWidth - 20)
          horizontalBox.setAlignment(Pos.TOP_RIGHT)
          bubble.setBubbleSpec(BubbleSpec.FACE_RIGHT_CENTER)
          horizontalBox.getChildren.addAll(bubble)
          horizontalBox
        } else {
          val bubble: BubbledLabel = new BubbledLabel
          bubble.setText(msg.sender + ": " + msg.message)
          bubble.setBackground(new Background(new BackgroundFill(Color.AQUAMARINE, null, null)))
          val horizontalBox = new HBox
          bubble.setBubbleSpec(BubbleSpec.FACE_LEFT_CENTER)
          horizontalBox.getChildren.addAll(bubble)
          horizontalBox
        }).foreach(hbox => chatPanel.getItems.add(hbox))
      }
    })
  }

  def updateMessageStory(storyMessage: Map[String,List[StringMessageFromServer]]) : Unit = storyMessageChat = storyMessage

}

class ActorViewController(clientRef : ActorRef, chatController : ChatController) extends Actor {

  override def preStart(): Unit = {
    clientRef ! Client.SetActorView(self)
  }

  override def receive: Receive = {
    case UpdateUserAndGroupActive(userList:List[String], groupList:List[String])=>
      chatController.updateUserGroupList(userList, groupList, None, None)
    case ResponseForChatCreation(response: Boolean) => Unit
    case UpdateStoryMessage(storyMessage : Map[String,List[StringMessageFromServer]],recipient : String) =>
      chatController.updateMessageStory(storyMessage)
      chatController.updateMessageView(recipient)
  }
}

object ActorViewController {

  /**
    * Get all users and chat group active
    * @param userList list of username
    * @param groupList list of chat group
    */
  final case class UpdateUserAndGroupActive(userList:List[String], groupList:List[String])

  /**
    * Response about request to create a one to one chat
    * @param accept
    */
  final case class ResponseForChatCreation(accept : Boolean)

  /**
    * Receives all messages received
    * @param storyMessage map which stores for every recipient (key) all messages received
    */
  final case class UpdateStoryMessage(storyMessage : Map[String,List[StringMessageFromServer]],recipient : String)
}
