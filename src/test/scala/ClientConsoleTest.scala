
import Client._
import akka.actor.{ActorSystem, ExtendedActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActor, TestActorRef, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientConsoleTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with a console/view" must {

    "if receives userName from login view, sends a registration request to server" in {
      val logInconsole = TestProbe("logInConsole")
      val clientTest = TestActorRef[Client](Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      //not empty username
      logInconsole.send(clientTest,Client.LogInFromConsole("userA"))//JoinRequest("userA") sent to server
      logInconsole.expectNoMessage()
      //empty username
      logInconsole.send(clientTest,Client.LogInFromConsole(""))//JoinRequest("") sent to server
      logInconsole.expectNoMessage()
    }

    "if receives request from console to create a one to one chat" in {

    }

    "if receives message from client console, it checks if recipient ref is stored" in {

      val serverTest = TestProbe("serverRegister")

      //log in from console
      val clientTestA = TestActorRef[Client](Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val clientConsoleA = TestProbe("logInConsoleA")
      clientConsoleA.send(clientTestA,Client.LogInFromConsole("userA"))//server receives JoinRequest("userA")
      clientConsoleA.expectNoMessage()
      serverTest.send(clientTestA,Client.AcceptRegistrationFromRegister(true))
      serverTest.expectMsg(RegisterServer.AllUsersAndGroupsRequest)

      val clientConsoleB = TestProbe("logInConsoleB")
      val clientTestB = TestActorRef[Client](Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      clientConsoleB.send(clientTestB,Client.LogInFromConsole("userB"))//server receives JoinRequest("userB")
      clientConsoleB.expectNoMessage()
      serverTest.send(clientTestB,Client.AcceptRegistrationFromRegister(true))
      serverTest.expectMsg(RegisterServer.AllUsersAndGroupsRequest)

      //first message from clientA: recipient isn't stored in client map(friendName,chatServer)
      clientConsoleA.send(clientTestA,RequestForChatCreationFromConsole("userA")) //server receives AllUsersAndGroupsRequest and NewOneToOneChatRequest
      clientConsoleA.expectNoMessage()
    }

    "if receives an attachment from client console" in {
      val clientTest = TestActorRef[Client](Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      clientTest.tell(AttachmentMessageFromConsole,self)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }

    "if receives a request from console of creating a new chat group and forward it to register" in {
      val clientTest = TestActorRef[Client](Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      clientTest.tell(CreateGroupRequestFromConsole("groupName"),self)
      expectNoMessage()
      //expectMsgClass(classOf[NewGroupChatRequest])
    }

    "if receives a request from console of joining to an existing chat group and forward it to register" in {
      val groupName = "TestNameGroup"
      val clientTest = TestActorRef[Client](Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      clientTest.tell(JoinGroupRequestFromConsole(groupName),self)
      expectNoMessage()
      //expectMsgClass(JoinGroupChatRequest(groupName).getClass)
    }
  }
}
