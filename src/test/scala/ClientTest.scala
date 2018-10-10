import Client._
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
      client.tell(AcceptRegistrationFromRegister,self)
      //expectNoMessage()
    }

    "Get users list and chat groups list" in {
      client.tell(UserAndGroupActive,self)
      //expectNoMessage()
    }

    "Receive message from a chat server" in {
      client.tell(StringMessageFromServer,self)
      //expectNoMessage()
    }

    "Receive message from client console" in {
      client.tell(StringMessageFromConsole,self)
      //expectNoMessage()
    }

    "Receive attachment from chat server" in {
      client.tell(AttachmentMessageFromServer,self)
      //expectNoMessage()
    }

    "Receive attachment from client console" in {
      client.tell(AttachmentMessageFromConsole,self)
      //expectNoMessage()
    }

    "receive response for chat creation" in {
      client.tell(ResponseForChatCreation,self)
      //expectNoMessage()
    }
  }

}
