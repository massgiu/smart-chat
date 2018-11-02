import Client._
import RegisterServer.{AllUsersAndGroupsRequest, JoinGroupChatRequest, NewGroupChatRequest}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientChatServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with ChatServer" must {
    val client = system.actorOf(Props[Client], name = "client")

    "Receive message from a chat server" in {
      client.tell(StringMessageFromServer("testMessage", 0, "sender"),self)
      expectNoMessage()
    }

    "Receive attachment from chat server" in {
      client.tell(AttachmentMessageFromServer(new AttachmentContent(), 0, "sender"),self)
      expectNoMessage()
    }

  }

}
