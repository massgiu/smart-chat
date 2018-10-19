import Client.{AcceptRegistrationFromRegister, UserAndGroupActive}
import RegisterServer.JoinRequest
import akka.actor.{ActorSystem, Props}
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
      clientOne.send(server, RegisterServer.JoinRequest(clientOne.toString))
      clientOne.expectMsgClass(AcceptRegistrationFromRegister(true).getClass)
      clientOne.send(server,RegisterServer.AllUsersAndGroupsRequest)
      clientOne.expectMsgClass(UserAndGroupActive(List("clientOne"),List()).getClass)
      //ClientTwo registration
      clientTwo.send(server, RegisterServer.JoinRequest(clientTwo.toString))
      clientTwo.expectMsgClass(AcceptRegistrationFromRegister(true).getClass)
      clientTwo.send(server,RegisterServer.AllUsersAndGroupsRequest)
      //Request from ClientOne to create a oneToOne chat with ClientTwo
      clientOne.send(server,RegisterServer.NewOneToOneChatRequest(clientOne.toString))

    }

  }

}
