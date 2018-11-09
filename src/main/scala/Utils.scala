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


