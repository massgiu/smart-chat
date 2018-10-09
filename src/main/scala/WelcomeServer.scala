class WelcomeServer {

}

object WelcomeServer {
  case class JoinRequest(clientName:String)
  case class NewOneToOneChatRequest(friendName:String)
  case class NewGroupChatRequest()
  case class JoinGroupChatRequest(group:String)
}
