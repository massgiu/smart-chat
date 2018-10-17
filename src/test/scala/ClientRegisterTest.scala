import Client._
import RegisterServer.{AllUsersAndGroupsRequest, JoinGroupChatRequest, NewGroupChatRequest}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientRegisterTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client whe interacts with register" must {
    val client = system.actorOf(Props[Client], name = "client")

    "Accept Registration From Register" in {
      client.tell(AcceptRegistrationFromRegister(true),self)
      expectNoMessage()
      client.tell(AcceptRegistrationFromRegister(false),self)
      expectNoMessage()
    }

    "Get users list and chat groups list" in {
      client.tell(UserAndGroupActive(List[String](),List[String]()),self)
      expectNoMessage()
    }

    "Receive response for chat creation" in {
      client.tell(ResponseForChatCreation(true, Option(self)),self)
      expectNoMessage()
      client.tell(ResponseForChatCreation(false,Option(self)),self)
      expectNoMessage()
    }
  }

}
