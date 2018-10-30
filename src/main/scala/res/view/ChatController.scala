package res.view

import java.net.URL
import java.util.ResourceBundle

import javafx.application.Application
import javafx.collections.{FXCollections, ObservableList}
import javafx.event.ActionEvent
import javafx.fxml.{FXMLLoader, Initializable}
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.stage.FileChooser

object LaunchClientView extends App {
  Application.launch(classOf[LaunchClientView], args: _*)
}

class LaunchClientView extends Application{
  override def start(primaryStage: Stage): Unit = {

    val root: Parent = FXMLLoader.load(getClass.getResource("/res/view/clientView.fxml"))
    val scene = new Scene(root)
    primaryStage.setTitle("Chat View")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

class ChatController extends Initializable{

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

  var userList: ObservableList[String] = _
  var groupList: ObservableList[String] = _

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

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
