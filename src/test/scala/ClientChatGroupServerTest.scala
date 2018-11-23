import Client._
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientChatGroupServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "ChatGroup Server when interacts with clients" must {

    "accept a client request to create chatGroup, accept the request of a second Client to join to it and then exchange messages between them" in {
      val clientOneName = "clientOne"
      val clientTwoName = "clientTwo"
      val clientThreeName = "clientThree"
      val clientOne = TestProbe(clientOneName)
      val clientTwo = TestProbe(clientTwoName)
      val clientThree = TestProbe(clientThreeName)
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer1")
      //ClientOne registration with username
      clientOne.send(server, RegisterServer.JoinRequest(clientOneName))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      //ClientTwo registration with username
      clientTwo.send(server, RegisterServer.JoinRequest(clientTwoName))
      clientTwo.expectMsg(AcceptRegistrationFromRegister(true))
      //ClientThree registration with username
      clientThree.send(server, RegisterServer.JoinRequest(clientThreeName))
      clientThree.expectMsg(AcceptRegistrationFromRegister(true))
      var expectedMessage = UserAndGroupActive(List(clientOneName, clientTwoName, clientThreeName), List.empty)
      clientOne.expectMsgAllConformingOf(Seq.fill(2)(classOf[UserAndGroupActive]):_*)
      clientOne.expectMsg(expectedMessage)
      clientTwo.expectMsgAllConformingOf(Seq.fill(1)(classOf[UserAndGroupActive]):_*)
      clientTwo.expectMsg(expectedMessage)
      clientThree.expectMsg(expectedMessage)
      //clientOne, clientTwo clientThree create new groups
      val groupNameOne = "groupOne"
      val groupNameTwo = "groupTwo"
      clientOne.send(server,RegisterServer.NewGroupChatRequest(groupNameOne))
      clientOne.expectMsg(Client.ResponseForChatCreation(true))
      expectedMessage = UserAndGroupActive(List(clientOneName, clientTwoName, clientThreeName), List(groupNameOne))
      clientOne.expectMsg(expectedMessage)
      clientTwo.expectMsg(expectedMessage)
      clientThree.expectMsg(expectedMessage)
      clientOne.send(server,RegisterServer.NewGroupChatRequest(groupNameOne))
      clientOne.expectMsg(Client.ResponseForChatCreation(false))
      clientTwo.send(server,RegisterServer.NewGroupChatRequest(groupNameOne))
      clientTwo.expectMsg(Client.ResponseForChatCreation(false))
      clientTwo.send(server,RegisterServer.NewGroupChatRequest(groupNameTwo))
      clientTwo.expectMsg(Client.ResponseForChatCreation(true))
      expectedMessage = UserAndGroupActive(List(clientOneName, clientTwoName, clientThreeName), List(groupNameOne,groupNameTwo))
      clientOne.expectMsg(expectedMessage)
      clientTwo.expectMsg(expectedMessage)
      clientThree.expectMsg(expectedMessage)
      //clientOne, clientTwo clientThree begin to chat after joining to groupOne
      clientOne.send(server, RegisterServer.GetGroupServerRef(groupNameOne))
      val chatGroupServerAForClientOne = clientOne.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      clientTwo.send(server, RegisterServer.GetGroupServerRef(groupNameOne))
      val chatGroupServerAForClienTwo = clientTwo.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      clientOne.send(chatGroupServerAForClientOne,GroupChatServer.AddMember(clientOneName,clientOne.ref))
      clientOne.expectMsg(ResponseForJoinGroupRequest(true,groupNameOne))
      clientTwo.send(chatGroupServerAForClienTwo,GroupChatServer.AddMember(clientTwoName,clientTwo.ref))
      clientTwo.expectMsg(ResponseForJoinGroupRequest(true,groupNameOne))
      clientTwo.send(chatGroupServerAForClienTwo,GroupChatServer.GroupMessage("groupOneMessage1",clientTwoName))
      clientOne.expectMsg(StringMessageFromGroupServer("groupOneMessage1", 1, clientTwoName, clientOneName))
      clientTwo.expectMsg(StringMessageFromGroupServer("groupOneMessage1", 1, clientTwoName, clientTwoName))
      clientOne.send(chatGroupServerAForClientOne,GroupChatServer.GroupMessage("groupOneMessage2",clientTwoName))
      clientOne.expectMsg(StringMessageFromGroupServer("groupOneMessage2", 2, clientTwoName, clientOneName))
      clientTwo.expectMsg(StringMessageFromGroupServer("groupOneMessage2", 2, clientTwoName, clientTwoName))
      clientTwo.send(chatGroupServerAForClienTwo,GroupChatServer.RemoveMember(clientTwoName))
      clientTwo.expectMsg(ResponseForUnJoinGroupRequest(true,groupNameOne))
      clientThree.send(server, RegisterServer.GetGroupServerRef(groupNameOne))
      val chatGroupServerAForClienThree = clientThree.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      clientThree.send(chatGroupServerAForClienThree,GroupChatServer.AddMember(clientThreeName,clientThree.ref))
      clientThree.expectMsg(ResponseForJoinGroupRequest(true,groupNameOne))
      clientThree.send(chatGroupServerAForClienThree,GroupChatServer.GroupMessage("groupOneMessage3",clientThreeName))
      clientOne.expectMsg(StringMessageFromGroupServer("groupOneMessage3", 3, clientThreeName, clientOneName))
      clientThree.expectMsg(StringMessageFromGroupServer("groupOneMessage3", 3, clientThreeName, clientThreeName))
      clientOne.send(chatGroupServerAForClientOne,GroupChatServer.RemoveMember(clientOneName))
      clientOne.expectMsg(ResponseForUnJoinGroupRequest(true,groupNameOne))
      clientThree.send(chatGroupServerAForClienThree,GroupChatServer.RemoveMember(clientThreeName))
      clientThree.expectMsg(ResponseForUnJoinGroupRequest(true,groupNameOne))
1    }
  }
}
