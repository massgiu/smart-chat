import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, UserAndGroupActive}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class RegisterServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Register Server" must {
    "accept a new client asking to join it" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer1")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
    }
    "refuse a new client asking to join it using another client's name" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer4")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(false))
    }
    "send a list of all users when a client asks it" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer2")
      server.tell(RegisterServer.AllUsersAndGroupsRequest, this.testActor)
      expectMsgClass(classOf[UserAndGroupActive])
    }
    "Respond to a client when it wants to create a new one-to-one chat" in {
      val clientName = "aClient"
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer3")
      server.tell(RegisterServer.JoinRequest(clientName), this.testActor)
      expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      server.tell(RegisterServer.NewOneToOneChatRequest(clientName), this.testActor)
      expectMsgClass(classOf[ResponseForChatCreation])
    }
  }
}