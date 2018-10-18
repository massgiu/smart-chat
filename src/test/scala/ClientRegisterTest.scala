import Client._
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ClientRegisterTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll{

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A client when interacts with register" must {
    val client = system.actorOf(Props[Client], name = "client")

    "Get the response about registration from Register" in {
      /*
      client.tell(AcceptRegistrationFromRegister(true),self)
      expectMsgClass(AllUsersAndGroupsRequest.getClass)
      client.tell(AcceptRegistrationFromRegister(false),self)
      expectNoMessage()
      */

      //Multiple request
      def createTestWithMultipleActor(numActor: Int): Unit = {
        def request(numTest:Int,listActor:List[Map[TestProbe,Boolean]]):List[Map[TestProbe,Boolean]] = numTest match {
          case 0 => listActor
          case _ => {
            val act = TestProbe(numTest.toString)
            val randomBooleanVar : Boolean = math.random <= 0.5
            act.send(client,Client.AcceptRegistrationFromRegister(randomBooleanVar))
            request(numTest-1,Map(act->randomBooleanVar) :: listActor)
          }
        }

        def testResponse(listActor:List[Map[TestProbe,Boolean]]):List[Map[TestProbe,Boolean]] = listActor.length match {
          case 0 => listActor
          case _ => {
            val headMap = listActor.head
            headMap(headMap.keys.head) match {
              case true => headMap.keys.head.expectMsgClass(RegisterServer.AllUsersAndGroupsRequest.getClass)
              case _ => headMap.keys.head.expectNoMessage()
            }
            testResponse(listActor.tail)
          }
        }
        testResponse(request(numActor,List[Map[TestProbe,Boolean]]()))
      }
      createTestWithMultipleActor(3)
    }

    "Get users list and chat groups list" in {
      client.tell(UserAndGroupActive(List[String]("myUserName"),List[String]()),self)
      expectNoMessage()
    }

    "Receive response for chat group creation" in {
      client.tell(ResponseForChatCreation(true, Option(self)),self)
      expectNoMessage()
      client.tell(ResponseForChatCreation(false,Option(self)),self)
      expectNoMessage()
    }
  }

}
