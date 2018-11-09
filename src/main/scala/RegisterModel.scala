import akka.actor.ActorRef

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
    val result = users.find(nameAndActorRef => nameAndActorRef._1 == name)
    OperationDone(result.isDefined, if (result.isDefined) List(result.get._2) else List.empty)
  }

  def findUserName(ref: ActorRef): OperationDone[String] = {
    val name = users.find(_._2 == ref).map(u => u._1)
    OperationDone(name.isDefined, if (name.isDefined) List(name.get) else List.empty)
  }

  def findGroup(name: String): OperationDone[ActorRef] = {
    val result = groups.find(nameAndActorRef => nameAndActorRef._1 == name)
    OperationDone(result.isDefined, if (result.isDefined) List(result.get._2) else List.empty)
  }

  def getChatsAsList: List[ActorRef] = {
    chats
  }

  def ifNewNameIsValidOrElse(name: String, ifValid: () => Unit, ifNotValid: () => Unit): Unit = {
    if (name != null && name.length > 0)
      ifValid()
    else
      ifNotValid()
  }

  object MapExtension {
    implicit class ExtendedMap(m: Map[String, ActorRef]) {
      def ifPresentOrElse(key: String, ifNotPresent: () => Unit, ifPresent: ((String, ActorRef)) => Unit): Unit = m.find(keyValue => keyValue._1 == key).fold(ifNotPresent())(ifPresent)
    }
  }

}
