import scala.reflect.ClassTag

/**
  * Created by noname on 11.03.16.
  */
case class Row[T](elems: T*)(implicit fractional: Fractional[T], tag: ClassTag[T]) {
  import fractional._
  import tag._

  val length = elems.length

  def *(a: T): Row[T] = Row(elems.map((x) => x * a): _*)

  def /(a: T): Row[T] = Row(elems.map((x) => x / a): _*)

  def apply(i: Int): T = elems(i)

  override def toString: String = elems.mkString("(", " ", ")")
}