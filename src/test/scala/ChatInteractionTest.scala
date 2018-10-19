import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, StringMessageFromServer, UserAndGroupActive}
import OneToOneChatServer.Message
import RegisterServer.JoinRequest
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ChatInteractionTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  override def beforeAll: Unit = {

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
      clientOne.send(server,RegisterServer.NewOneToOneChatRequest("clientTwo"))

      val testchatServer = clientOne.expectMsgPF()({
        case ResponseForChatCreation(true, oneToOneServer) if oneToOneServer.isDefined => oneToOneServer.get
      })

      //Start chat bewtween two users
      val firstMessageText = "msgFromClientOne"
      clientOne.send(testchatServer,Message(firstMessageText))
      val firstMessageIndex = clientTwo.expectMsgPF()({
        case StringMessageFromServer(`firstMessageText`, messageNumber) => messageNumber
      })
      clientOne.expectMsgPF()({
        case StringMessageFromServer(`firstMessageText`, `firstMessageIndex`) => Unit
      })

      val secondMessageText = "msgFromClientTwo"
      val secondMessageIndex = firstMessageIndex + 1
      clientTwo.send(testchatServer,Message(secondMessageText))
      clientOne.expectMsgPF()({
        case StringMessageFromServer(`secondMessageText`, `secondMessageIndex`) => Unit
      })
      clientTwo.expectMsgPF()({
        case StringMessageFromServer(`secondMessageText`, `secondMessageIndex`) => Unit
      })
    }

  }

}
