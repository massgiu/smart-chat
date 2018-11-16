
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
import javafx.scene.control.Alert.AlertType
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.stage.{FileChooser, Stage}

class LaunchClientView extends Application{
  override def start(primaryStage: Stage): Unit = {
    val system = ActorSystem.create("MySystem",ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
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
  var groupListView : ListView[String] = _
  @FXML
  var userListView : ListView[String] = _
  @FXML
  var usernameLabel : Label = _
  @FXML
  var chatPanel : ListView[String] = _
  @FXML
  var onlineCountLabel : Label = _
  @FXML
  var newChatGroupButton : Button = _

  var chatMessage : ObservableList[String] = FXCollections.observableArrayList()
  var storyMessageChat : Map[String,List[StringMessageFromServer]] = Map.empty

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    system.actorOf(Props(classOf[ActorViewController],clientRef,this))
    usernameLabel.setText(userName)
  }

  @FXML
  def userSelected(): Unit = {
    clientRef ! Client.RequestForChatCreationFromConsole(userListView.getSelectionModel.getSelectedItem)
    updateMessageView(userListView.getSelectionModel.getSelectedItem)
  }

  @FXML
  def groupSelected(): Unit = {
    val dialog = new Alert(AlertType.CONFIRMATION)
    dialog.setTitle("Confirmation Dialog")
    dialog.setHeaderText("Do you confirm to add to chatGroup: " +groupListView.getSelectionModel.getSelectedItem)
    import javafx.scene.control.ButtonType
    val result = dialog.showAndWait
    if (result.get() == ButtonType.OK) println("Request to add to chatGroup: "+ groupListView.getSelectionModel.getSelectedItem)
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
    if (userListView.getSelectionModel.getSelectedItem!=null) {
      clientRef ! Client.StringMessageFromConsole(messageBox.getText(), userListView.getSelectionModel.getSelectedItem)
      messageBox.clear()
    }
    else if (groupListView.getSelectionModel.getSelectedItem!=null) {
      clientRef ! Client.StringMessageFromConsole(messageBox.getText(), groupListView.getSelectionModel.getSelectedItem)
      messageBox.clear()
    }
  }

  def attachmentButtonAction(event : ActionEvent) : Unit = {

    val fileChooser = new FileChooser
    val selectFile = fileChooser.showOpenDialog(null)
    if (selectFile != null) {
      val filePath =  selectFile.getAbsolutePath
      println(filePath)
    }
    else println("File is not valid")
  }

  def updateUserGroupList(users:List[String], groups:List[String]) : Unit ={
    Platform.runLater(()=>  {
      var convertoToObservable : util.ArrayList[String] = new util.ArrayList[String]()
      users.filter(elem=>elem!=userName).foreach(elem=>convertoToObservable.add(elem))
      val userList: ObservableList[String] = FXCollections.observableArrayList(convertoToObservable)
      userListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      userListView.setItems(userList)
      onlineCountLabel.setText(userList.size().toString)

      convertoToObservable.clear()
      groups.foreach(elem=>convertoToObservable.add(elem))
      var groupList: ObservableList[String] = FXCollections.observableArrayList(convertoToObservable)
      groupListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      groupListView.setItems(groupList)
    })
  }

  def updateMessageView(recipient: String): Unit ={
    Platform.runLater(()=>{
      if (storyMessageChat.contains(recipient)) {
        chatMessage.clear()
        val allMessageForRecipient = storyMessageChat(recipient)
        allMessageForRecipient.foreach(elem => chatMessage.add(elem.sender + ": " + elem.message))
        chatPanel.setItems(chatMessage)
      } else {
        chatMessage.clear()
        chatPanel.setItems(chatMessage)
      }
    })
  }

  def updateMessageStory(storyMessage: Map[String,List[StringMessageFromServer]]) : Unit = {
    storyMessageChat = storyMessage
  }

}

class ActorViewController(clientRef : ActorRef, chatController : ChatController) extends Actor {

  override def preStart(): Unit = {
    clientRef ! Client.SetActorView(self)
  }

  override def receive: Receive = {
    case UpdateUserAndGroupActive(userList:List[String], groupList:List[String])=>
      chatController.updateUserGroupList(userList, groupList)
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
