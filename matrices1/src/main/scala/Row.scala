import scala.reflect.ClassTag

/**
  * Created by noname on 11.03.16.
  */
case class Row[T](e: T*)(implicit fractional: Fractional[T], tag: ClassTag[T]) {
  import fractional._
  import tag._

  val elems = e.toArray

  def this(e: Array[T])(implicit fractional: Fractional[T], tag: ClassTag[T]) =
    this(e: _*)

  def this(dim: Int)(implicit fractional: Fractional[T], tag: ClassTag[T]) = {
    this(new Array[T](dim))
  }

  val length = elems.length

  def *(a: T): Row[T] = Row(elems.map((x) => x * a): _*)

  def /(a: T): Row[T] = Row(elems.map((x) => x / a): _*)

  def +(r: Row[T]): Row[T] = Row(elems.zip(r.elems).map { case (x,y) => x+y } : _*)

  def -(r: Row[T]): Row[T] = Row(elems.zip(r.elems).map { case (x,y) => x-y } : _*)
  def apply(i: Int): T = elems(i)

  def norm: T = elems.maxBy(abs(_))

  override def toString: String = elems.mkString("(", " ", ")")
}
