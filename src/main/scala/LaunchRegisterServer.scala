import java.io.File

import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object LaunchRegisterServer extends App {
  val config = ConfigFactory.parseFile(new File("src/main/scala/res/server.conf"))
  val system = ActorSystem.create("MySystem", config)
  system.actorOf(Props(new RegisterServer(system)), name = "server")
}
