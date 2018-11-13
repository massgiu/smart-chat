import akka.actor.ActorRef
import Utils._

class RegisterModel {

  private var users: Map[String, ActorRef] = Map.empty
  private var groups: Map[String, ActorRef] = Map.empty
  private var chats: List[ActorRef] = List.empty
  val invalidName = "" //this must be consistent with the result of ifNewNameIsValidOrElse

  def addNewUser(clientName: String, clientRef: ActorRef): EmptyOperationDone = {
    var success = false
    ifNewNameIsValidOrElse(clientName, () => users.find(_._1 == clientName).fold({
      users += (clientName -> clientRef)
      success = true
    })(_ => Unit), () => Unit)
    EmptyOperationDone(success)
  }

  def addNewOneToOneChatServer(newChatServer: ActorRef): Unit = {
    chats = newChatServer :: chats
  }

  def addNewGroupChatServer(newGroupName: String, newGroupChatServer: ActorRef): Unit = {
    groups += (newGroupName -> newGroupChatServer)
  }

  def removeUser(clientName: String): Unit = {
    users -= clientName
  }

  def getAllUsersAndGroupsNames: OperationDone[(List[String], List[String])] = {
    val result = Option((users.keys.toList, groups.keys.toList))
    OperationDone(result.isDefined, if (result.isDefined) List(result.get) else List.empty)
  }

  def findUser(name: String): OperationDone[ActorRef] = {
    findInMap(name, users)
  }

  def findUserName(ref: ActorRef): OperationDone[String] = {
    reverseFindInMap(ref, users)
  }

  def findGroup(name: String): OperationDone[ActorRef] = {
    findInMap(name, groups)
  }

  def findGroupName(ref: ActorRef): OperationDone[String] = {
    reverseFindInMap(ref, groups)
  }

  def getChatsAsList: List[ActorRef] = {
    chats
  }

}