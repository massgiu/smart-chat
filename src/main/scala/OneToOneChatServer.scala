class OneToOneChatServer {

}

object OneToOneChatServer {

  case class message(text:String)
  case class attachment(payload:Attachment)
}

class Attachment {

}
