import Client.{AcceptRegistrationFromRegister, ResponseForChatCreation, StringMessageFromServer, UserAndGroupActive}
import OneToOneChatServer.Message
import RegisterServer.{JoinGroupChatRequest, JoinRequest}
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
      val clientWithoutUserName = TestProbe("clientWithoutUserName")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer")

      //ClientOne registration with username
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.send(server,RegisterServer.AllUsersAndGroupsRequest)
      clientOne.expectMsg(UserAndGroupActive(List("clientOne"),List()))

      //ClientTwo registration with username
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

      //Request from ClientOne to create a chatGroup
      clientOne.send(server, RegisterServer.NewGroupChatRequest("chatGroupName"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))

      //Request from ClientOne to join to chatGroup name "chatGroupName"
      clientOne.send(server, RegisterServer.JoinGroupChatRequest("chatGroupName"))
      clientOne.expectMsgPF()({
        case ResponseForChatCreation(true, chatGroupServer) => Unit
      })

      //Request from ClientTwo to join to chatGroup name "chatGroupName"
      clientTwo.send(server, RegisterServer.JoinGroupChatRequest("chatGroupName"))
        clientTwo.expectMsgPF()({
        case ResponseForChatCreation(true, chatGroupServer) => Unit
      })

      //clientWithoutUserName registration
      clientWithoutUserName.send(server,RegisterServer.JoinRequest(null))
      clientWithoutUserName.expectMsg(AcceptRegistrationFromRegister(true))

      //ClientOne registration with same username
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(false))

      //ClientOne create a oneToOnechatGroup with an inexistent user
      clientOne.send(server,RegisterServer.NewOneToOneChatRequest("inexistentClient"))
      clientOne.expectMsgPF()({
        case ResponseForChatCreation(false, oneToOneServer) => Unit
      })

      //ClientOne create a chatGroup with same group name
      clientOne.send(server, RegisterServer.NewGroupChatRequest("chatGroupName"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(false))

      //ClientOne request to join to an inexistent chat group
      clientOne.send(server, RegisterServer.JoinGroupChatRequest("inexistentChatGroup"))
      clientOne.expectMsgPF()({
        case ResponseForChatCreation(false, chatGroupServer) => Unit
      })

    }
  }
}
