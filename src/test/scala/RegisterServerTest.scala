import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, UserAndGroupActive}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class RegisterServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Register Server" must {
    "accept a new client asking to join it and add it to the register" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer1")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
      server.tell(RegisterServer.AllUsersAndGroupsRequest, this.testActor)
      expectMsg(UserAndGroupActive(List("name"),List()))
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
      val first = TestProbe("first")
      val second = TestProbe("second")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer3")
      first.send(server, RegisterServer.JoinRequest(first.toString))
      first.expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      second.send(server, RegisterServer.JoinRequest(second.toString))
      second.expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      first.send(server, RegisterServer.NewOneToOneChatRequest(second.toString))
      first.expectMsgPF()({
        case ResponseForChatCreation(true, actor) if actor.isDefined => Unit
      })
    }
    "Respond to a client when it wants to create a new group chat" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer5")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
      server.tell(RegisterServer.NewGroupChatRequest("groupName"), this.testActor)
      expectMsgPF()({
        case ResponseForChatCreation(true, actor) if actor.isDefined => Unit
      })
      server.tell(RegisterServer.NewGroupChatRequest("groupName"), this.testActor)
      expectMsgPF()({
        case ResponseForChatCreation(false, actor) if actor.isEmpty => Unit
      })
    }
  }
}