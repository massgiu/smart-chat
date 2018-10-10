import java.util

import Client._
import OneToOneChatServer.attachment
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client actor" must {
    val client = system.actorOf(Props[Client], name = "client")

    "Accept Registration From Register" in {
      client.tell(AcceptRegistrationFromRegister(true),self)
      expectNoMessage()
      client.tell(AcceptRegistrationFromRegister(false),self)
      expectNoMessage()
    }

    "Get users list and chat groups list" in {
      client.tell(UserAndGroupActive(List[String](),List[String]()),self)
      expectNoMessage()
    }

    "Receive message from a chat server" in {
      client.tell(StringMessageFromServer("testMessage"),self)
      //expectNoMessage()
    }

    "Receive message from client console" in {
      client.tell(StringMessageFromConsole("testMessage"),self)
      expectMsgClass(message("testMessage").getClass)
    }

    "Receive attachment from chat server" in {
      client.tell(AttachmentMessageFromServer,self)
      //expectNoMessage()
    }

    "Receive attachment from client console" in {
      client.tell(AttachmentMessageFromConsole,self)
      expectMsgClass(attachment.getClass)
    }

    "Receive response for chat creation" in {
      client.tell(ResponseForChatCreation(true),self)
      expectNoMessage()
      client.tell(ResponseForChatCreation(false),self)
      expectNoMessage()
    }
  }

}
