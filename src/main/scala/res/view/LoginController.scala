package res.view

import akka.actor.ActorRef
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.fxml.FXMLLoader
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

object LaunchClientLogin extends App {
  Application.launch(classOf[LaunchClientLogin], args: _*)
}

class LaunchClientLogin extends Application{
  override def start(primaryStage: Stage): Unit = {

    val root: Parent = FXMLLoader.load(getClass().getResource("/res/view/clientLogin.fxml"))

    val scene = new Scene(root)
    primaryStage.setTitle("Client Login")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

class LoginController() {

  import javafx.fxml.FXML

  @FXML
  var closeButton : Button = _
  @FXML
  var connectButton : Button = _
  @FXML
  var messageLabel : Label = _
  @FXML
  var usernameTextfield : TextField = _

  def loginButtonAction(event:ActionEvent): Unit ={
    println(usernameTextfield.getText)
    //Send Message to clientref
  }

  def closeButtonAction(event:ActionEvent): Unit ={
    val stage = closeButton.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
  }
}
