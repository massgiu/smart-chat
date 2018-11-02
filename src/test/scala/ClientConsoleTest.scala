
import Client._
import akka.actor.{ActorSystem, ExtendedActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientConsoleTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with a console/view" must {

    "if receives userName from login view, sends a registration request to server" in {
      val logInconsole = TestProbe("logInConsole")
      val serverTest = TestActorRef[RegisterServer](Props(new RegisterServer))
      //not empty username
      logInconsole.send(client,Client.LogInFromConsole("userNameForTest"))
      serverTest.receive(RegisterServer.JoinRequest("userNameForTest"))
      //empty username
      logInconsole.send(client,Client.LogInFromConsole(""))
      expectNoMessage()
    }

    "if receives message from client console" in {
      val clientConsole = TestProbe("clientConsole")
      client.tell(StringMessageFromConsole("testMessage", "recipient"),self)
      //A message is sent to ChatServer
      expectNoMessage()
    }

    "if receives an attachment from client console" in {
      client.tell(AttachmentMessageFromConsole,self)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }

    "if receives a request from console of creating a new chat group and forward it to register" in {
      client.tell(CreateGroupRequestFromConsole("groupName"),self)
      expectNoMessage()
      //expectMsgClass(classOf[NewGroupChatRequest])
    }

    "if receives a request from console of joining to an existing chat group and forward it to register" in {
      val groupName = "TestNameGroup"
      client.tell(JoinGroupRequestFromConsole(groupName),self)
      expectNoMessage()
      //expectMsgClass(JoinGroupChatRequest(groupName).getClass)
    }
  }
}
