import Client._
import RegisterServer._
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class RegisterServerTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Register Server" must {
    "accept a new client asking to join it and add it to the register" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer1")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
      expectMsgClass(classOf[UserAndGroupActive])
      server.tell(RegisterServer.AllUsersAndGroupsRequest, this.testActor)
      expectMsg(UserAndGroupActive(List("name"),List()))
    }
    "refuse a new client asking to join it using another client's name" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer4")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
      expectMsgClass(classOf[UserAndGroupActive])
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(false))
    }
    "send a list of all users when a client asks it" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer2")
      server.tell(RegisterServer.AllUsersAndGroupsRequest, this.testActor)
      expectMsgClass(classOf[UserAndGroupActive])
    }
    "delete a client when it wants to leave" in {
      val first = TestProbe("first")
      val second = TestProbe("second")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer11")
      first.send(server, RegisterServer.JoinRequest("first"))
      first.expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      first.expectMsgClass(classOf[UserAndGroupActive])
      second.send(server, RegisterServer.JoinRequest("second"))
      second.expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      second.expectMsgClass(classOf[UserAndGroupActive])
      first.expectMsgClass(classOf[UserAndGroupActive])
      second.send(server, AllUsersAndGroupsRequest)
      second.expectMsg(UserAndGroupActive(List("first", "second"), List.empty))
      first.send(server, Unjoin())
      second.send(server, AllUsersAndGroupsRequest)
      second.expectMsg(UserAndGroupActive(List("second"), List.empty))
    }
    "Respond to a client when it wants to create a new one-to-one chat" in {
      val first = TestProbe("first")
      val second = TestProbe("second")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer3")
      first.send(server, RegisterServer.JoinRequest(first.toString))
      first.expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      first.expectMsgClass(classOf[UserAndGroupActive])
      second.send(server, RegisterServer.JoinRequest(second.toString))
      second.expectMsgClass(classOf[Client.AcceptRegistrationFromRegister])
      second.expectMsgClass(classOf[UserAndGroupActive])
      first.expectMsgClass(classOf[UserAndGroupActive])
      first.send(server, RegisterServer.NewOneToOneChatRequest(second.toString))
      first.expectMsg(ResponseForChatCreation(true))
    }
    "Respond to a client when it wants to create a new group chat" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer5")
      server.tell(RegisterServer.JoinRequest("name"), this.testActor)
      expectMsg(AcceptRegistrationFromRegister(true))
      expectMsgClass(classOf[UserAndGroupActive])
      server.tell(RegisterServer.NewGroupChatRequest("groupName"), this.testActor)
      expectMsg(ResponseForChatCreation(true))
      expectMsg(UserAndGroupActive(List("name"),List("groupName")))
      server.tell(RegisterServer.NewGroupChatRequest("groupName"), this.testActor)
      expectMsg(ResponseForChatCreation(false))
    }
    "refuse a client request, without user name, to register to server" in {
      val clientWithoutUserName = TestProbe("clientWithoutUserName")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer9")
      clientWithoutUserName.send(server, RegisterServer.JoinRequest(null))
      clientWithoutUserName.expectMsg(AcceptRegistrationFromRegister(false))
      clientWithoutUserName.send(server, RegisterServer.JoinRequest(""))
      clientWithoutUserName.expectMsg(AcceptRegistrationFromRegister(false))
    }
    "refuse a client request to create an oneToOne chat with an unregistered user" in {
      val clientOne = TestProbe("clientOne")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer6")
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      //ClientOne create a oneToOnechatGroup with an inexistent user
      clientOne.send(server, RegisterServer.NewOneToOneChatRequest("unregisteredUser"))
      clientOne.expectMsg(ResponseForChatCreation(false))
    }
    "refuse a client request to create a group chat with a name already used" in {
      val clientOne = TestProbe("clientOne")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer7")
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      //ClientOne create a chatGroup with same group name
      clientOne.send(server, RegisterServer.NewGroupChatRequest("chatGroupName"))
      clientOne.expectMsg(ResponseForChatCreation(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      clientOne.send(server, RegisterServer.NewGroupChatRequest("chatGroupName"))
      clientOne.expectMsg(ResponseForChatCreation(false))
    }
    "refuse a client request to join to a chat group with an inexisting name" in {
      val clientOne = TestProbe("clientOne")
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer8")
      clientOne.send(server, RegisterServer.JoinRequest("clientOne"))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne.expectMsgClass(classOf[UserAndGroupActive])
      //ClientOne request to join to an inexistent chat group
//      clientOne.send(server, RegisterServer.JoinGroupChatRequest("inexistentChatGroup"))
//      clientOne.expectMsg(ResponseForChatCreation(false))
    }
    "delete a chat server when one of its members leaves and unregister him" in {
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer12")
      val clientOne = ("one", TestProbe("one"))
      val clientTwo = ("two", TestProbe("two"))
      clientOne._2.send(server, RegisterServer.JoinRequest(clientOne._1))
      clientOne._2.expectMsg(AcceptRegistrationFromRegister(true))
      clientOne._2.expectMsgClass(classOf[UserAndGroupActive])
      clientTwo._2.send(server, RegisterServer.JoinRequest(clientTwo._1))
      clientTwo._2.expectMsg(AcceptRegistrationFromRegister(true))
      clientTwo._2.expectMsgClass(classOf[UserAndGroupActive])
      clientOne._2.expectMsgClass(classOf[UserAndGroupActive])
      clientOne._2.send(server, NewOneToOneChatRequest(clientTwo._1))
      clientOne._2.expectMsg(ResponseForChatCreation(true))
      clientTwo._2.send(server, GetServerRef(clientOne._1))
      clientTwo._2.expectMsgPF()({case ResponseForServerRefRequest(response) if response.isDefined => Unit})
      clientOne._2.send(server, Unjoin())
      clientTwo._2.expectMsg(UserAndGroupActive(List(clientTwo._1), List.empty))
      clientTwo._2.send(server, GetServerRef(clientOne._1))
      clientTwo._2.expectMsgPF()({case ResponseForServerRefRequest(response) if response.isEmpty => Unit})
    }
    "send an empty option when a client asks for a non-existing chat server and send the right one when two chat server are present" in {
      val nameOne = "clientOne"
      val nameTwo = "clientTwo"
      val nameThree = "clientThree"
      val nameFour = "clientFour"
      val clientOne = TestProbe(nameOne)
      val clientTwo = TestProbe(nameTwo)
      val clientThree = TestProbe(nameThree)
      val clientFour = TestProbe(nameFour)
      val server = system.actorOf(Props[RegisterServer], name = "welcomeServer10")

      //clients register to server
      clientOne.send(server, JoinRequest(nameOne))
      clientOne.expectMsg(AcceptRegistrationFromRegister(true))
      clientTwo.send(server, JoinRequest(nameTwo))
      clientTwo.expectMsg(AcceptRegistrationFromRegister(true))
      clientThree.send(server, JoinRequest(nameThree))
      clientThree.expectMsg(AcceptRegistrationFromRegister(true))
      clientFour.send(server, JoinRequest(nameFour))
      clientFour.expectMsg(AcceptRegistrationFromRegister(true))

      val expectedMessage = UserAndGroupActive(List(nameOne, nameTwo, nameThree, nameFour), List.empty)
      clientOne.expectMsgAllConformingOf(Seq.fill(3)(classOf[UserAndGroupActive]):_*)
      clientOne.expectMsg(expectedMessage)
      clientTwo.expectMsgAllConformingOf(Seq.fill(2)(classOf[UserAndGroupActive]):_*)
      clientTwo.expectMsg(expectedMessage)
      clientThree.expectMsgAllConformingOf(Seq.fill(1)(classOf[UserAndGroupActive]):_*)
      clientThree.expectMsg(expectedMessage)
      clientFour.expectMsgAllConformingOf(Seq.fill(0)(classOf[UserAndGroupActive]):_*)
      clientFour.expectMsg(expectedMessage)

      //clientOne tries to get two non-existing chat servers and to create an already-existing chat server
      clientOne.send(server, GetServerRef(nameTwo))
      clientOne.expectMsg(ResponseForServerRefRequest(Option.empty))
      clientOne.send(server, NewOneToOneChatRequest(nameTwo))
      clientOne.expectMsg(ResponseForChatCreation(true))
      clientOne.send(server, NewOneToOneChatRequest(nameTwo))
      clientOne.expectMsg(ResponseForChatCreation(false))
      clientOne.send(server, GetServerRef("clientFive"))
      clientOne.expectMsg(ResponseForServerRefRequest(Option.empty))

      //clientThree creates a new chat with clientFour, and the relative chat server is different from the one
      //used by clientOne and clientTwo
      clientOne.send(server, GetServerRef(nameTwo))
      val serverClientOneAndTwo = clientOne.expectMsgPF()({case ResponseForServerRefRequest(chatServer) => chatServer})
      clientThree.send(server, NewOneToOneChatRequest(nameFour))
      clientThree.expectMsg(ResponseForChatCreation(true))
      clientThree.send(server, GetServerRef(nameFour))
      val serverClientThreeAndFour = clientThree.expectMsgPF()({case ResponseForServerRefRequest(chatServer) => chatServer})
      println("OneAndTwo: " + serverClientOneAndTwo)
      println("ThreeAndFour: " + serverClientThreeAndFour)
      assert(serverClientOneAndTwo != serverClientThreeAndFour)
    }
  }
}