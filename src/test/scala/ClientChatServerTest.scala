import java.nio.file.{Files, Paths}

import ActorViewController.{UpdateStoryComboGroupMessage, UpdateStoryComboMessage, UpdateUserAndGroupActive}
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

    "Send string messages to its UI according to their ordering" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val view = TestProbe("view")
      client ! SetActorView(view.ref)
      view.expectMsgClass(classOf[UpdateUserAndGroupActive])
      client ! StringMessageFromServer("message1", 1, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromServer("message2", 3, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromServer("message4", 5, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromServer("message3", 2, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 3 => Unit})
      client ! StringMessageFromServer("message2", 4, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 5 => Unit})
    }

    "Send attachments to its UI according to their ordering" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val attachMentTest = new Array[Byte](1)
      val view = TestProbe("view")
      client ! SetActorView(view.ref)
      view.expectMsgClass(classOf[UpdateUserAndGroupActive])
      client ! AttachmentMessageFromServer(attachMentTest, 1, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 3, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 5, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 2, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 3 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 4, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 5 => Unit})
    }

    "Send messages and attachments to its UI according to their ordering" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val attachMentTest = new Array[Byte](1)
      val view = TestProbe("view")
      client ! SetActorView(view.ref)
      view.expectMsgClass(classOf[UpdateUserAndGroupActive])
      client ! StringMessageFromServer("message1", 1, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 3, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromServer("message5", 5, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 2, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 3 => Unit})
      client ! AttachmentMessageFromServer(attachMentTest, 4, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboMessage(msgs, _) if msgs("sender").length == 5 => Unit})
    }

    "Send group string messages to its UI according to their ordering" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val view = TestProbe("view")
      client ! SetActorView(view.ref)
      view.expectMsgClass(classOf[UpdateUserAndGroupActive])
      client ! StringMessageFromGroupServer("message1", 1, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromGroupServer("message2", 3, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromGroupServer("message4", 5, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromGroupServer("message3", 2, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 3 => Unit})
      client ! StringMessageFromGroupServer("message2", 4, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 5 => Unit})
    }

    "Send group messages and group attachments to its UI according to their ordering" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val attachMentTest = new Array[Byte](1)
      val view = TestProbe("view")
      client ! SetActorView(view.ref)
      view.expectMsgClass(classOf[UpdateUserAndGroupActive])
      client ! StringMessageFromGroupServer("message1", 1, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromGroupServer(attachMentTest, 3, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! StringMessageFromGroupServer("message5", 5, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 1 => Unit})
      client ! AttachmentMessageFromGroupServer(attachMentTest, 2, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 3 => Unit})
      client ! AttachmentMessageFromGroupServer(attachMentTest, 4, "sender", "recipient")
      view.expectMsgPF()({case UpdateStoryComboGroupMessage(msgs, _) if msgs("sender").length == 5 => Unit})
    }
  }
}
