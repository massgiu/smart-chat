import Client.AcceptRegistrationFromRegister
import WelcomeServer.JoinRequest
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client actor" must {
    val client = system.actorOf(Props[Client], name = "pong")

    "Accept Registration From Register" in {
      client.tell(JoinRequest,self)
      expectMsgClass(AcceptRegistrationFromRegister.getClass)
    }
  }

}
