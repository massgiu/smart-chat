
import java.io.File

import Client._
import akka.actor.{ActorSystem, ExtendedActorSystem, Props, Terminated}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}


class ClientConsoleTest extends TestKit(ActorSystem.create("MySystem", ConfigFactory.parseFile(new File("src/main/scala/res/server.conf")))) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with a console/view" must {

    "if receives userName from login view, sends a registration request to server" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val serverA = system.actorOf(Props(new RegisterServer()), name = "server")

      val consoleForClient = TestProbe("actorFromConsole")
      val testActor = TestProbe("testActor")

      //not empty username: the username inserted by console is registered
      consoleForClient.send(client,Client.LogInFromConsole("userA"))//JoinRequest("userA") sent to server
      consoleForClient.expectNoMessage()
      testActor.send(serverA,RegisterServer.JoinRequest("userB"))//to get user and group list, applicant must be registered
      testActor.expectMsg(Client.AcceptRegistrationFromRegister(true))
      testActor.send(serverA,RegisterServer.AllUsersAndGroupsRequest)
      var userList = testActor.expectMsgPF()({
        case (UserAndGroupActive(users,_))=> users
      })
      assert(userList.contains("userA"))

      //empty username: the username inserted by console is registered
      consoleForClient.send(client,Client.LogInFromConsole(""))//JoinRequest("") sent to server
      consoleForClient.expectNoMessage()
      testActor.send(serverA,RegisterServer.AllUsersAndGroupsRequest)
      userList = testActor.expectMsgPF()({
        case (UserAndGroupActive(userList,groupList))=> userList
      })
      assert(userList.length==2)

      val StopServerActorTest = TestProbe()
      system.stop(client)
      StopServerActorTest.watch(serverA)
      system.stop(serverA)
      StopServerActorTest.expectTerminated(serverA)
    }

    "if receives request from console to create a one to one chat" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val serverB = system.actorOf(Props(new RegisterServer()), name = "server")

      val consoleForClient = TestProbe("actorFromConsole")
      val testActor = TestProbe("testActor")

      consoleForClient.send(client, Client.LogInFromConsole("userA")) //JoinRequest("userC") sent to server
      consoleForClient.expectNoMessage()
      testActor.send(serverB, RegisterServer.JoinRequest("userB"))
      testActor.expectMsg(Client.AcceptRegistrationFromRegister(true))
      consoleForClient.send(client, Client.RequestForChatCreationFromConsole("userB"))
      consoleForClient.expectNoMessage()
      testActor.send(serverB, RegisterServer.GetServerRef("userA"))
      val testchatServer = testActor.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      testActor.send(testchatServer, OneToOneChatServer.Message("messageToUserA"))
      testActor.expectMsg(Client.StringMessageFromServer("messageToUserA", 1, "userB"))

      val StopServerActorTest = TestProbe()
      system.stop(client)
      StopServerActorTest.watch(serverB)
      system.stop(serverB)
      StopServerActorTest.expectTerminated(serverB)

    }

    "if receives message from client console, it checks if recipient chatServer ref is stored and sends to chatserver" in {
      val clientA = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val serverC = system.actorOf(Props(new RegisterServer()), name = "server")

      val consoleForClientA = TestProbe("actorFromConsoleA")
      val testActor = TestProbe("testActor")

      consoleForClientA.send(clientA,Client.LogInFromConsole("userA"))//JoinRequest("userE") sent to server
      consoleForClientA.expectNoMessage()
      testActor.send(serverC,RegisterServer.JoinRequest("userC"))
      testActor.expectMsg(Client.AcceptRegistrationFromRegister(true))
      consoleForClientA.send(clientA,Client.RequestForChatCreationFromConsole("userC"))
      consoleForClientA.expectNoMessage()
      testActor.send(clientA,Client.StringMessageFromConsole("messagetoUserA","userC"))
      testActor.expectMsg(Client.StringMessageFromServer("messagetoUserA",1,"userA"))

      val StopServerActorTest = TestProbe()
      system.stop(clientA)
      StopServerActorTest.watch(serverC)
      system.stop(serverC)
      StopServerActorTest.expectTerminated(serverC)
    }

    "if receives an attachment from client console" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActor = TestProbe("testActor")
      testActor.send(client,Client.AttachmentMessageFromConsole)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }

    "if receives a request from console of creating a new chat group and forward it to register" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActor = TestProbe("testActor")
      testActor.send(client,Client.CreateGroupRequestFromConsole("groupName"))
      expectNoMessage()
      //expectMsgClass(classOf[NewGroupChatRequest])
    }

    "if receives a request from console of joining to an existing chat group and forward it to register" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActor = TestProbe("testActor")
      testActor.send(client,Client.JoinGroupRequestFromConsole("TestNameGroup"))
      expectNoMessage()
      //expectMsgClass(JoinGroupChatRequest(groupName).getClass)
    }
  }
}
