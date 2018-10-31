import java.io.File

import akka.actor.{ActorSystem, ExtendedActorSystem, Props}
import com.typesafe.config.ConfigFactory

object LauchClient extends App {
  val config = ConfigFactory.parseFile(new File("src/main/scala/res/client.conf"))
  val system = ActorSystem.create("MySystem", config)
  system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
}
