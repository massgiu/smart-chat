
import java.io.File
import java.net.URL
import java.util
import java.util.ResourceBundle

import ActorViewController.{MessageFromClient, ResponseForChatCreation, UpdateUserAndGroupActive}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import javafx.application.{Application, Platform}
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.ActionEvent
import javafx.fxml.Initializable
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
  var closeButton : Button = _
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

  var chatMessage : ObservableList[String] = FXCollections.observableArrayList()

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    system.actorOf(Props(classOf[ActorViewController],clientRef,this))
    usernameLabel.setText(userName)
  }

  @FXML
  def userSelected(): Unit = {
    clientRef ! Client.RequestForChatCreationFromConsole(userListView.getSelectionModel.getSelectedItem())
  }

  @FXML
  def groupSelected(): Unit = {
    println(groupListView.getSelectionModel.getSelectedItem())
  }

  def closeButtonAction(event:ActionEvent): Unit ={
    val stage = closeButton.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
  }

  def sendButtonAction(event:ActionEvent): Unit = {
    clientRef ! Client.StringMessageFromConsole(messageBox.getText(),userListView.getSelectionModel.getSelectedItem())
    messageBox.clear()
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

  def updateUserGroupList(users:List[String], groups:List[String]) ={
    Platform.runLater(()=>  {
      var convertoToObservable : util.ArrayList[String] = new util.ArrayList[String]()
      users.filter(elem=>(elem!=userName)).foreach(elem=>convertoToObservable.add(elem))
      val userList: ObservableList[String] = FXCollections.observableArrayList(convertoToObservable)
      userListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      userListView.setItems(userList)

      convertoToObservable.clear()
      groups.foreach(elem=>convertoToObservable.add(elem))
      var groupList: ObservableList[String] = FXCollections.observableArrayList(convertoToObservable)
      groupListView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
      groupListView.setItems(groupList)
    })
  }

  def updateMessageView(message: String, messageNumber :Long, senderName: String): Unit ={
    Platform.runLater(()=>{
      chatMessage.add(senderName + ": " + message)
      chatPanel.setItems(chatMessage)
    })
  }

}

class ActorViewController(clientRef : ActorRef, chatController : ChatController) extends Actor {

  override def preStart(): Unit = {
    clientRef ! Client.SetActorView(Option(self))
  }

  override def receive: Receive = {
    case UpdateUserAndGroupActive(userList:List[String], groupList:List[String])=> {
      chatController.updateUserGroupList(userList, groupList)
    }
    case ResponseForChatCreation(response: Boolean) =>{

    }
    case MessageFromClient(message:String, messageNumber:Long, senderName:String) => {
      chatController.updateMessageView(message,messageNumber,senderName)
    }
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
    *
    * @param message message to display
    * @param messageNumber progressive number of chat message
    * @param senderName sender of message
    */
  final case class MessageFromClient(message:String, messageNumber:Long, senderName:String)
}
