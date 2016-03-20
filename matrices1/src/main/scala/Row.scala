/**
  * Created by noname on 11.03.16.
  */
class Row[T](elems: Array[T])(implicit numeric: Numeric[T]) extends Cloneable {
  import numeric._

  val length = elems.length

  def *(a: T): Row[T] = {
    new Row(elems.map((x) => x * a))
  }

  override def clone: Row[T] = new Row[T](elems.clone)
}