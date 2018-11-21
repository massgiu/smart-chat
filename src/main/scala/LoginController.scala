
import java.io.File

import ActorLoginController.ResponseFromLogin
import Client.StopRequest
import Utils.interactionWithUI
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.Scene
import javafx.scene.control.{Button, Label, TextField}
import javafx.stage.Stage

class LaunchClientLogin extends Application {

  override def start(primaryStage: Stage): Unit = {
    val system = ActorSystem.create("MySystem", ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
    val clientRef = system.actorOf(Props(classOf[Client], system))
    primaryStage.setOnCloseRequest(_ => clientRef ! StopRequest)
    //login GUI
    val loaderLogin: FXMLLoader = new FXMLLoader(getClass.getResource("/res/view/clientLogin.fxml"))
    val loginController = new LoginController(clientRef, system)
    loaderLogin.setController(loginController)
    val sceneLogin = new Scene(loaderLogin.load())
    primaryStage.setTitle("Login Client View")
    primaryStage.setScene(sceneLogin)
    primaryStage.show()
  }
}

class LoginController(clientRef : ActorRef, system : ActorSystem) {

  @FXML
  var connectButton : Button = _
  @FXML
  var messageLabel : Label = _
  @FXML
  var usernameTextfield : TextField = _

  def loginButtonAction(event:ActionEvent): Unit ={
    if (usernameTextfield.getText.length>0) system.actorOf(Props(classOf[ActorLoginController],
      usernameTextfield.getText, clientRef, this)) else loginRefuse()
  }

  def loginAccepted(userName: String): Unit = {
    val loaderChat: FXMLLoader = new FXMLLoader(getClass.getResource("/res/view/clientView.fxml"))
    loaderChat.setController(new ChatController(userName, clientRef, system))
    usernameTextfield.getScene.getWindow.asInstanceOf[Stage].setScene(new Scene(loaderChat.load()))
  }

  def loginRefuse(): Unit ={
    messageLabel.setText("Invalid userName")
  }
}

class ActorLoginController(userName: String, clientRef : ActorRef,loginController : LoginController ) extends Actor {

  override def preStart(): Unit = {
    clientRef ! Client.SetActorLogin(self)
    clientRef ! Client.LogInFromConsole(userName)
  }

  override def receive: Receive = {
    case ResponseFromLogin(accept : Boolean) => if (accept) {
      interactionWithUI {
        loginController.loginAccepted(userName)
      }
    } else {
      interactionWithUI {
        loginController.loginRefuse()
      }
    }
  }
}

object ActorLoginController {
  final case class ResponseFromLogin(accept : Boolean)
}

