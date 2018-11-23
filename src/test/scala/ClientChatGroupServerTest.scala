import Client.{AcceptRegistrationFromRegister, ResponseForJoinGroupRequest, UserAndGroupActive}
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

      //clientOne and clientTwo create new groups
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
    }
  }
}
