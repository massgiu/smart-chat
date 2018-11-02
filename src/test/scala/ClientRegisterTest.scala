import java.io.File

import Client._
import akka.actor.{ActorSystem, ExtendedActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientRegisterTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  val client = system.actorOf(Props(new Client(system.asInstanceOf[ExtendedActorSystem])))

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with register" must {

    "Get the response about registration from Register" in {
      /*
      client.tell(AcceptRegistrationFromRegister(true),self)
      expectMsgClass(AllUsersAndGroupsRequest.getClass)
      client.tell(AcceptRegistrationFromRegister(false),self)
      expectNoMessage()
      */

      //Multiple request
      def createTestWithMultipleActor(numActor: Int): Unit = {
        def request(numTest:Int,listActor:List[(TestProbe,Boolean)]):List[(TestProbe,Boolean)] = numTest match {
          case a:Int if a> 0 => {
            val act = TestProbe(numTest.toString)
            val randomBooleanVar : Boolean = math.random <= 0.5
            act.send(client,Client.AcceptRegistrationFromRegister(randomBooleanVar))
            request(numTest-1,(act,randomBooleanVar) :: listActor)
          }
          case _ => listActor
        }

        def testResponse(listActor:List[(TestProbe,Boolean)]):List[(TestProbe,Boolean)] = listActor.length match {
          case 0 => listActor
          case _ => {
            val tupleHead = listActor.head
            tupleHead._2 match {
              case true => tupleHead._1.expectMsgClass(RegisterServer.AllUsersAndGroupsRequest.getClass)
              case _ => tupleHead._1.expectNoMessage()
            }
            testResponse(listActor.tail)
          }
        }
        testResponse(request(numActor,List[(TestProbe,Boolean)]()))
      }
      createTestWithMultipleActor(10)
    }

    "Get users list and chat groups list" in {
      client.tell(UserAndGroupActive(List[String]("myUserName"),List[String]()),self)
      expectNoMessage()
    }

    "Receive response for chat group creation" in {
      client.tell(ResponseForChatCreation(true),self)
      expectNoMessage()
      client.tell(ResponseForChatCreation(false),self)
      expectNoMessage()
    }
  }

}
