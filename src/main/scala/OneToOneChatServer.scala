class OneToOneChatServer {

}

object OneToOneChatServer {

  case class Message(text:String)
  case class Attachment(payload:Attachment)
}

class Attachment {

}
