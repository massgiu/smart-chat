import java.util

import Client._
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientConsoleTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with a console/view" must {
    val client = system.actorOf(Props[Client], name = "client")

    "Receive message from client console" in {
      client.tell(StringMessageFromConsole("testMessage"),self)
      //A message is sent to ChatServer
      expectNoMessage()
    }

    "Receive attachment from client console" in {
      client.tell(AttachmentMessageFromConsole,self)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }
  }

}
