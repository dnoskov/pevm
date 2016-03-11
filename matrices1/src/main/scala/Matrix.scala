import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by noname on 10.03.16.
  */

object Matrix {

}


case class Matrix[T](elems: Row[T]*)(implicit fractional: Fractional[T]) {
  import fractional._

  val dim = (elems.length, elems(0).length)

//  def det: T = {
//    var result = zero
//
//    for (i <- 0 until dim._1) {
//      val normrow = this(i) / this(i,i)
//    }
//
//    result
//  }

  def apply(i: Int): Row[T] = elems(i)

  def apply(i: Int, j: Int): T = elems(i)(j)

  def swap(rows: (Int, Int)): Matrix[T] = {
    var newelems: mutable.WrappedArray[Row[T]] = elems.toArray.clone
    val tmprow = elems(rows._1)
    newelems(rows._1) = newelems(rows._2)
    newelems(rows._2) = tmprow
    Matrix(newelems: _*)
  }

  override def toString: String = {
    val result = (for (r <- elems) yield r.toString).mkString("\n")
    result
  }
}


