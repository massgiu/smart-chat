
import java.io.File

import Client._
import akka.actor.{ActorSystem, ExtendedActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActor, TestActorRef, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientConsoleTest extends TestKit(ActorSystem("MySystem")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with a console/view" must {

    implicit val system = ActorSystem.create("MySystem", ConfigFactory.parseFile(new File("src/main/scala/res/server.conf")))
    val server = system.actorOf(Props(new RegisterServer()), name = "server")

    "if receives userName from login view, sends a registration request to server" in {
      implicit val system = ActorSystem.create("MySystem", ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))

      val logInconsoleA = TestProbe("logInConsoleA")
      val testActorA = TestProbe("testActorA")

      //not empty username: the username inserted by console is registered
      logInconsoleA.send(client,Client.LogInFromConsole("userA"))//JoinRequest("userA") sent to server
      logInconsoleA.expectNoMessage()
      testActorA.send(server,RegisterServer.JoinRequest("userB"))//to get user and group list, applicant must be registered
      testActorA.expectMsg(Client.AcceptRegistrationFromRegister(true))
      testActorA.send(server,RegisterServer.AllUsersAndGroupsRequest)
      var userList = testActorA.expectMsgPF()({
        case (UserAndGroupActive(userList,groupList))=> userList
      })
      assert(userList.contains(("userA")))

      //empty username: the username inserted by console is registered
      logInconsoleA.send(client,Client.LogInFromConsole(""))//JoinRequest("") sent to server
      logInconsoleA.expectNoMessage()
      testActorA.send(server,RegisterServer.AllUsersAndGroupsRequest)
      userList = testActorA.expectMsgPF()({
        case (UserAndGroupActive(userList,groupList))=> userList
      })
      assert(userList.length==2)
    }

    "if receives request from console to create a one to one chat" in {
      implicit var system = ActorSystem.create("MySystem", ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))

      val logInconsoleB = TestProbe("logInConsoleB")
      val testActorB = TestProbe("testActorB")

      logInconsoleB.send(client,Client.LogInFromConsole("userC"))//JoinRequest("userC") sent to server
      logInconsoleB.expectNoMessage()
      testActorB.send(server,RegisterServer.JoinRequest("userD"))
      testActorB.expectMsg(Client.AcceptRegistrationFromRegister(true))
      logInconsoleB.send(client,Client.RequestForChatCreationFromConsole("userD"))
      logInconsoleB.expectNoMessage()
      testActorB.send(server,RegisterServer.GetServerRef("userC"))
      val testchatServer = testActorB.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      testActorB.send(testchatServer,OneToOneChatServer.Message("messageToUserC"))
      testActorB.expectMsg(Client.StringMessageFromServer("messageToUserC",1,"userD"))
    }

    "if receives message from client console, it checks if recipient chatServer ref is stored and sends to chatserver" in {
      implicit var system = ActorSystem.create("MySystem", ConfigFactory.parseFile(new File("src/main/scala/res/client.conf")))
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))

      val logInconsoleC = TestProbe("logInConsoleC")
      val testActorC = TestProbe("testActorC")

      logInconsoleC.send(client,Client.LogInFromConsole("userE"))//JoinRequest("userE") sent to server
      logInconsoleC.expectNoMessage()
      testActorC.send(server,RegisterServer.JoinRequest("userF"))
      testActorC.expectMsg(Client.AcceptRegistrationFromRegister(true))
      logInconsoleC.send(client,Client.RequestForChatCreationFromConsole("userF"))
      logInconsoleC.expectNoMessage()
      testActorC.send(client,Client.StringMessageFromConsole("messagetoUserE","userF"))
      testActorC.expectMsg(Client.StringMessageFromServer("messagetoUserE",1,"userE"))
    }

    "if receives an attachment from client console" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActorD = TestProbe("testActorD")
      testActorD.send(client,Client.AttachmentMessageFromConsole)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }

    "if receives a request from console of creating a new chat group and forward it to register" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActorE = TestProbe("testActorE")
      testActorE.send(client,Client.CreateGroupRequestFromConsole("groupName"))
      expectNoMessage()
      //expectMsgClass(classOf[NewGroupChatRequest])
    }

    "if receives a request from console of joining to an existing chat group and forward it to register" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActorF = TestProbe("testActorF")
      testActorF.send(client,Client.JoinGroupRequestFromConsole("TestNameGroup"))
      expectNoMessage()
      //expectMsgClass(JoinGroupChatRequest(groupName).getClass)
    }
  }
}
