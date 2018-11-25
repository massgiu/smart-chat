import Client._
import akka.actor.{ActorSystem, ExtendedActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientChatServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with ChatServer" must {

    "Receive message from a chat server" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      client.tell(StringMessageFromServer("testMessage", 0, "sender", "client"),self)
      expectNoMessage()
    }

    "Receive attachment from chat server" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      client.tell(AttachmentMessageFromServer(new Array[Byte](0), 0, "sender","recipient"),self)
      expectNoMessage()
    }

//    "Send messages to its UI according to their ordering" in {
//      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
//      val view = TestProbe("view")
//      client ! SetActorView(view.ref)
//      view.expectMsgClass(classOf[UpdateUserAndGroupActive])
//      client ! StringMessageFromServer("message1", 1, "sender", "recipient")
//      view.expectMsgPF()({case UpdateStoryMessage(msgs, _) if msgs("sender").length == 1 => Unit})
//      client ! StringMessageFromServer("message2", 3, "sender", "recipient")
//      view.expectMsgPF()({case UpdateStoryMessage(msgs, _) if msgs("sender").length == 1 => Unit})
//      client ! StringMessageFromServer("message4", 5, "sender", "recipient")
//      view.expectMsgPF()({case UpdateStoryMessage(msgs, _) if msgs("sender").length == 1 => Unit})
//      client ! StringMessageFromServer("message3", 2, "sender", "recipient")
//      view.expectMsgPF()({case UpdateStoryMessage(msgs, _) if msgs("sender").length == 3 => Unit})
//      client ! StringMessageFromServer("message2", 4, "sender", "recipient")
//      view.expectMsgPF()({case UpdateStoryMessage(msgs, _) if msgs("sender").length == 5 => Unit})
//    }

  }

}
