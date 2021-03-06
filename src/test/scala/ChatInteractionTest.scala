import Client._
import OneToOneChatServer.Message
import RegisterServer.{ GetServerRef}
import akka.actor.{ ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ChatInteractionTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "Server when interacts with clients" must {

    "accept clients request to register themselves and give back users list and chat group list" in {
      val clientOne = TestProbe("clientOne")
      val clientTwo = TestProbe("clientTwo")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer1")
      //ClientOne registration with username
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.send(server, RegisterServer.AllUsersAndGroupsRequest)
      clientOne.expectMsg(UserAndGroupActive(List("clientOne"), List()))

      //ClientTwo registration with username
      clientTwo.send(server, RegisterServer.JoinRequest("clientTwo"))
      clientTwo.expectMsg(AcceptRegistrationFromRegister(true))
      clientTwo.send(server, RegisterServer.AllUsersAndGroupsRequest)
      clientTwo.expectMsg(UserAndGroupActive(List("clientOne", "clientTwo"), List()))
    }

    "accept a client request to create an oneToOneChat, accept the request of a second Client to join to it and then exchange messages between them" in {
      val nameOne = "clientOne"
      val nameTwo = "clientTwo"
      val clientOne = TestProbe(nameOne)
      val clientTwo = TestProbe(nameTwo)
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer2")

      clientOne.send(server, RegisterServer.JoinRequest(nameOne))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      clientTwo.send(server, RegisterServer.JoinRequest(nameTwo))
      clientTwo.expectMsg(AcceptRegistrationFromRegister(true))
      clientTwo.expectMsgClass(classOf[UserAndGroupActive])
      clientOne.expectMsgClass(classOf[UserAndGroupActive])

      //Request from ClientOne to create a oneToOne chat with ClientTwo
      clientOne.send(server, RegisterServer.NewOneToOneChatRequest(nameTwo))
      clientOne.expectMsg(ResponseForChatCreation(true))

      clientOne.send(server, GetServerRef(nameTwo))
      val testchatServer = clientOne.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })

      //Start chat bewtween two users
      val firstMessageText = "msgFromClientOne"
      clientOne.send(testchatServer, Message(firstMessageText))
      val firstMessageIndex = clientTwo.expectMsgPF()({
        case StringMessageFromServer(`firstMessageText`, messageNumber, `nameOne`, `nameTwo`) => messageNumber
      })
      clientOne.expectMsgPF()({
        case StringMessageFromServer(`firstMessageText`, `firstMessageIndex`, `nameOne`, `nameTwo`) => Unit
      })

      val secondMessageText = "msgFromClientTwo"
      val secondMessageIndex = firstMessageIndex + 1
      clientTwo.send(testchatServer, Message(secondMessageText))
      clientOne.expectMsgPF()({
        case StringMessageFromServer(`secondMessageText`, `secondMessageIndex`, `nameTwo`, `nameOne`) => Unit
      })
      clientTwo.expectMsgPF()({
        case StringMessageFromServer(`secondMessageText`, `secondMessageIndex`, `nameTwo`, `nameOne`) => Unit
      })
    }

    "accept a client request to create a chat group and a second client request to join to it" in {
      val clientOne = TestProbe("clientOne")
      val clientTwo = TestProbe("clientTwo")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer3")

      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      clientTwo.send(server, RegisterServer.JoinRequest("clientTwo"))
      clientTwo.expectMsg(AcceptRegistrationFromRegister(true))
      clientTwo.expectMsgClass(classOf[UserAndGroupActive])
      clientOne.expectMsgClass(classOf[UserAndGroupActive])

      //Request from ClientOne to create a chatGroup
      clientOne.send(server, RegisterServer.NewGroupChatRequest("chatGroupName"))
      clientOne.expectMsg(ResponseForChatCreation(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      clientTwo.expectMsgClass(classOf[UserAndGroupActive])
      //Request from ClientOne to join to chatGroup named "chatGroupName"
      clientOne.send(server, RegisterServer.GetGroupServerRef("chatGroupName"))
      val groupChatServerForClientOne = clientOne.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      clientOne.send(groupChatServerForClientOne, GroupChatServer.AddMember("clientOne",clientOne.ref))
      clientOne.expectMsg(ResponseForJoinGroupRequest(true,"chatGroupName"))
      //Request from ClientTwo to join to chatGroup named "chatGroupName"
      clientTwo.send(server, RegisterServer.GetGroupServerRef("chatGroupName"))
      val groupChatServerForClientTwo = clientTwo.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      clientTwo.send(groupChatServerForClientTwo, GroupChatServer.AddMember("clientTwo",clientTwo.ref))
      clientTwo.expectMsg(ResponseForJoinGroupRequest(true,"chatGroupName"))
    }
  }
}
