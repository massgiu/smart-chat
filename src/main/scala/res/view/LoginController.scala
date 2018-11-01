package res.view

import akka.actor.{ActorRef, ActorSelection, ActorSystem}
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Scene
import javafx.scene.control.{Button, Label, TextField}
import javafx.stage.Stage

/*
object LaunchClientLogin extends App {
  Application.launch(classOf[LaunchClientLogin], args: _*)
}
*/

class LaunchClientLogin extends Application{

    override def start(primaryStage: Stage): Unit = {
    val clientRef = getParameters.getUnnamed.get(0)
    val loader : FXMLLoader = new FXMLLoader(getClass.getResource("/res/view/clientLogin.fxml"))
    loader.setController(new LoginController(clientRef))
    val scene = new Scene(loader.load())
    primaryStage.setTitle("Login Client View")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

class LoginController(clientRefToString : String) {

  @FXML
  var closeButton : Button = _
  @FXML
  var connectButton : Button = _
  @FXML
  var messageLabel : Label = _
  @FXML
  var usernameTextfield : TextField = _

  def loginButtonAction(event:ActionEvent): Unit ={
    val clientRef : ActorRef = clientRefToString.asInstanceOf[ActorRef]
    //clientRef.tell(Client.LogInFromConsole("TEst"),ActorRef.noSender)
  }

  def closeButtonAction(event:ActionEvent): Unit ={
    val stage = closeButton.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
  }

}
