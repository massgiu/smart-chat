import java.util

import Client._
import OneToOneChatServer.Message
import RegisterServer.{AllUsersAndGroupsRequest, JoinGroupChatRequest, NewGroupChatRequest}
import akka.actor.{Actor, ActorPath, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client" must {
    val client = system.actorOf(Props[Client], name = "client")

    "Accept Registration From Register" in {
      client.tell(AcceptRegistrationFromRegister(true),self)
      expectMsgClass(AllUsersAndGroupsRequest.getClass)
      client.tell(AcceptRegistrationFromRegister(false),self)
      expectNoMessage()
    }

    "Get users list and chat groups list" in {
      client.tell(UserAndGroupActive(List[String](),List[String]()),self)
      expectNoMessage()
    }

    "Receive message from a chat server" in {
      client.tell(StringMessageFromServer("testMessage"),self)
      expectNoMessage()
    }

    "Receive message from client console" in {
      client.tell(StringMessageFromConsole("testMessage"),self)
      //A message is sent to ChatServer
      expectNoMessage()
    }

    "Receive attachment from chat server" in {
      client.tell(AttachmentMessageFromServer,self)
      expectNoMessage()
    }

    "Receive attachment from client console" in {
      client.tell(AttachmentMessageFromConsole,self)
      //An attachment is sent to ChatServer
      expectNoMessage()
    }

    "Receive a request from console of creating a new chat group and forward it to register" in {
      client.tell(CreateGroupRequestFromConsole("groupName"),self)
      expectMsgClass(classOf[NewGroupChatRequest])
    }

    "Receive a request from console of joining to an existing chat group and forward it to register" in {
      val groupName = "TestNameGroup"
      client.tell(JoinGroupRequestFromConsole(groupName),self)
      expectMsgClass(JoinGroupChatRequest(groupName).getClass)
    }

    "Receive response for chat creation" in {
      client.tell(ResponseForChatCreation(true, Option(self)),self)
      expectNoMessage()
      client.tell(ResponseForChatCreation(false,Option(self)),self)
      expectNoMessage()
    }
  }

}
