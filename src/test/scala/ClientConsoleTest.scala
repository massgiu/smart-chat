import java.util

import Client._
import RegisterServer.{JoinGroupChatRequest, NewGroupChatRequest}
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

    "Receive a request from console of creating a new chat group and forward it to register" in {
      client.tell(CreateGroupRequestFromConsole("groupName"),self)
      expectNoMessage()
    }

    "Receive a request from console of joining to an existing chat group and forward it to register" in {
      val groupName = "TestNameGroup"
      client.tell(JoinGroupRequestFromConsole(groupName),self)
      expectNoMessage()
    }
  }

}
