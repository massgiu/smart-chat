import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, UserAndGroupActive}
import RegisterServer.JoinRequest
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ChatInteractionTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A right interaction between client and server" must {

    "accept a new client asking to join it and add it to the register" in {

      val clientOne = TestProbe("clientOne")
      val clientTwo = TestProbe("clientTwo")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer")

      //ClientOne registration
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.send(server,RegisterServer.AllUsersAndGroupsRequest)
      clientOne.expectMsg(UserAndGroupActive(List("clientOne"),List()))
      //ClientTwo registration

      clientTwo.send(server, RegisterServer.JoinRequest("clientTwo"))
      clientTwo.expectMsg(AcceptRegistrationFromRegister(true))
      clientTwo.send(server,RegisterServer.AllUsersAndGroupsRequest)
      clientTwo.expectMsg(UserAndGroupActive(List("clientOne","clientTwo"),List()))

      //Request from ClientOne to create a oneToOne chat with ClientTwo
      clientOne.send(server,RegisterServer.NewOneToOneChatRequest("clientOne"))
      clientOne.expectMsgPF()({
        case ResponseForChatCreation(false, actor) if actor.isDefined => Unit
      })

    }

  }

}
