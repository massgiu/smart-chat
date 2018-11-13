import MapExtension._

object Utils {

  def findInMap[A, B](key: A, map: Map[A, B]): OperationDone[B] = {
    map.ifKeyPresentOrElse(key, () => OperationDone(success = false, List.empty), keyAndValue => OperationDone(success = true, List(keyAndValue._2)))
  }

  def reverseFindInMap[A, B](value: B, map: Map[A, B]): OperationDone[A] = {
    map.ifValuePresentOrElse(value, () => OperationDone(success = false, List.empty), keyAndValue => OperationDone(success = true, List(keyAndValue._1)))
  }

  def ifNewNameIsValidOrElse(name: String, ifValid: () => Unit, ifNotValid: () => Unit): Unit = {
    if (name != null && name.length > 0)
      ifValid()
    else
      ifNotValid()
  }
}

class OperationDone[A](success: Boolean, result: List[A]) {
  def ifSuccess(action: List[A] => Unit): OperationDone[A] = {if (success) action(result); this}
  def orElse(action: List[A] => Unit): Unit = if (!success) action(result)
}

object OperationDone {
  def apply[A](success: Boolean, result: List[A] = List.empty): OperationDone[A] = new OperationDone(success, result)
}

class EmptyOperationDone(success: Boolean) extends OperationDone[Unit](success, List.empty)

object EmptyOperationDone {
  def apply(success: Boolean): EmptyOperationDone = new EmptyOperationDone(success)
}

object MapExtension {
  implicit class ExtendedMap[B, C](m: Map[B, C]) {
    def ifKeyPresentOrElse[A](key: B, ifNotPresent: () => A, ifPresent: ((B, C)) => A): A = {
      m.find(keyValue => keyValue._1 == key).fold(ifNotPresent())(ifPresent)
    }
    def ifValuePresentOrElse[A](value: C, ifNotPresent: () => A, ifPresent: ((B, C)) => A): A = {
      m.find(keyValue => keyValue._2 == value).fold(ifNotPresent())(ifPresent)
    }
  }
}


