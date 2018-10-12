import Client.{ResponseForChatCreation, UserAndGroupActive}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class RegisterServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Register Server" must {
    "send an answer to a client asking to join it" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer")
      server.tell(RegisterServer.JoinRequest, this.testActor)
      expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
    }
    "send a list of all users when a client asks it" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer")
      server.tell(RegisterServer.AllUsersAndGroupsRequest, this.testActor)
      expectMsgClass(classOf[UserAndGroupActive])
    }
    "Respond to a client when it wants to create a new one-to-one chat" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer")
      server.tell(RegisterServer.NewOneToOneChatRequest, this.testActor)
      expectMsgClass(classOf[ResponseForChatCreation])
    }
  }
}