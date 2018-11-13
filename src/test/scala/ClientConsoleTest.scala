
import java.io.File

import ActorLoginController.ResponseFromLogin
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

    "send a registration request to server, if receives userName from login view" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val serverA = system.actorOf(Props(new RegisterServer()), name = "server")
      val consoleForClient = TestProbe("actorFromConsole")
      val testActor = TestProbe("testActor")
      val nameOne = "userA"
      val nameTwo = "userB"

      //not empty username: the username inserted by console is registered
      consoleForClient.send(client,Client.SetActorLogin(consoleForClient.ref))
      consoleForClient.send(client,Client.LogInFromConsole(nameOne))//JoinRequest("userA") sent to server
      consoleForClient.expectMsg(ResponseFromLogin(true))
      testActor.send(serverA,RegisterServer.JoinRequest(nameTwo))//to get user and group list, applicant must be registered
      testActor.expectMsg(Client.AcceptRegistrationFromRegister(true))
      testActor.send(serverA,RegisterServer.AllUsersAndGroupsRequest)
      var userList = testActor.expectMsgPF()({
        case (UserAndGroupActive(users,_))=> users
      })
      assert(userList.contains(nameOne))

      //empty username: the username inserted by console is registered
      consoleForClient.send(client,Client.LogInFromConsole(""))//JoinRequest("") sent to server
      consoleForClient.expectMsg(ResponseFromLogin(false))
      testActor.send(serverA,RegisterServer.AllUsersAndGroupsRequest)
      userList = testActor.expectMsgPF()({
        case (UserAndGroupActive(userList,_))=> userList
      })
      assert(userList.length==2)

      val StopServerActorTest = TestProbe()
      system.stop(client)
      StopServerActorTest.watch(serverA)
      system.stop(serverA)
      StopServerActorTest.expectTerminated(serverA)
    }

    "create a oneToOne chat, if receives request from console" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val serverB = system.actorOf(Props(new RegisterServer()), name = "server")
      val consoleForClient = TestProbe("actorFromConsole")
      val testActor = TestProbe("testActor")
      val messageText = "messageToUserA"
      val nameOne = "userA"
      val nameTwo = "userB"

      consoleForClient.send(client,Client.SetActorLogin(consoleForClient.ref))
      consoleForClient.send(client, Client.LogInFromConsole(nameOne)) //JoinRequest("userA") sent to server
      consoleForClient.expectMsg(ResponseFromLogin(true))
      testActor.send(serverB, RegisterServer.JoinRequest(nameTwo))
      testActor.expectMsg(Client.AcceptRegistrationFromRegister(true))
      testActor.expectMsgClass(classOf[UserAndGroupActive])
      consoleForClient.send(client, Client.RequestForChatCreationFromConsole(nameTwo))
      consoleForClient.expectNoMessage()
      testActor.send(serverB, RegisterServer.GetServerRef(nameOne))
      val testChatServer = testActor.expectMsgPF()({
        case ResponseForServerRefRequest(serverOpt) if serverOpt.isDefined => serverOpt.get
      })
      testActor.send(testChatServer, OneToOneChatServer.Message(messageText))
      testActor.expectMsg(Client.StringMessageFromServer(messageText, 1, nameTwo))

      val StopServerActorTest = TestProbe()
      system.stop(client)
      StopServerActorTest.watch(serverB)
      system.stop(serverB)
      StopServerActorTest.expectTerminated(serverB)

    }

    "check if chatServer ref is stored and send a message to chatserver, if receives message from client console" in {
      val serverC = system.actorOf(Props(new RegisterServer()), name = "server")
      val clientA = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val consoleForClientA = TestProbe("actorFromConsoleA")
      val clientB = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val consoleForClientB = TestProbe("actorFromConsoleB")
      val testActor = TestProbe("testActor")
      val messageTextOne = "messageToUserC"
      val messageTextTwo = "messageToUserB"
      val nameOne = "userA"
      val nameTwo = "userB"
      val nameThree = "userC"

      consoleForClientA.send(clientA,Client.SetActorLogin(consoleForClientA.ref))
      consoleForClientA.send(clientA,Client.LogInFromConsole(nameOne))//JoinRequest("userA") sent to server
      consoleForClientA.expectMsg(ResponseFromLogin(true))
      testActor.send(serverC,RegisterServer.JoinRequest(nameThree))
      testActor.expectMsg(Client.AcceptRegistrationFromRegister(true))
      testActor.expectMsgClass(classOf[UserAndGroupActive])
      consoleForClientA.send(clientA,Client.RequestForChatCreationFromConsole(nameThree))
      consoleForClientA.expectNoMessage()
      consoleForClientA.send(clientA,Client.StringMessageFromConsole(messageTextOne,nameThree))
      testActor.expectMsg(Client.StringMessageFromServer(messageTextOne,1,nameOne)) //chatServer sends msg to sender

      consoleForClientB.send(clientB,Client.SetActorLogin(consoleForClientB.ref))
      consoleForClientB.send(clientB,Client.LogInFromConsole(nameTwo))//JoinRequest("userB") sent to server
      consoleForClientB.expectMsg(ResponseFromLogin(true))
      consoleForClientA.send(clientA,Client.RequestForChatCreationFromConsole(nameTwo))
      consoleForClientA.expectNoMessage()
      consoleForClientA.send(clientA,Client.StringMessageFromConsole(messageTextTwo,nameTwo))
      consoleForClientA.expectNoMessage()

      val StopServerActorTest = TestProbe()
      system.stop(clientA)
      system.stop(clientB)
      StopServerActorTest.watch(serverC)
      system.stop(serverC)
      StopServerActorTest.expectTerminated(serverC)
    }

    "send an attachment to chatServer, if receives an attachment from client console" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActor = TestProbe("testActor")
      testActor.send(client,Client.AttachmentMessageFromConsole)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }

    "send the request to register, if receives a request from console " in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActor = TestProbe("testActor")
      testActor.send(client,Client.CreateGroupRequestFromConsole("groupName"))
      expectNoMessage()
      //expectMsgClass(classOf[NewGroupChatRequest])
    }

    "send the request to register if receives a request from console of joining to an existing chat" in {
      val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))
      val testActor = TestProbe("testActor")
      expectNoMessage()
      //expectMsgClass(JoinGroupChatRequest(groupName).getClass)
    }
  }
}
