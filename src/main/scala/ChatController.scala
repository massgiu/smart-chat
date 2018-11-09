
import java.io.File
import java.net.URL
import java.util.ResourceBundle

import akka.actor.{Actor, ActorRef, ActorSelection, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import javafx.application.Application
import javafx.collections.ObservableList
import javafx.event.ActionEvent
import javafx.fxml.{Initializable}
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.{Parent, Scene}
import javafx.stage.{FileChooser, Stage}

class LaunchClientView extends Application{
  override def start(primaryStage: Stage): Unit = {
    val system = ActorSystem.create("MySystem",ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
    val actorController = system.actorOf(Props(new ActorViewController()))
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

  var userList: ObservableList[String] = _
  var groupList: ObservableList[String] = _

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
      usernameLabel.setText(userName)
  }

  @FXML
  def userSelected(): Unit = {
    println(userListView.getSelectionModel.getSelectedItem())
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
    val text = messageBox.getText()
    println(text)
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

}

class ActorViewController extends Actor {
  override def receive: Receive = {
    case _ => Unit
  }
}
