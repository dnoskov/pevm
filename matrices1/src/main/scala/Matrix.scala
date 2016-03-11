/**
  * Created by noname on 10.03.16.
  */

object Matrix {

}


class Matrix[T](elems: Array[Row[T]])(implicit numeric: Numeric[T]) {
  import numeric._

  val dim = (elems.length, elems(0).length)

//  def det: T = {
//
//  }

  def swap(rows: (Int, Int)): Matrix[T] = {
    var newelems = elems.clone()
    val tmprow = elems(rows._1).clone
    newelems(rows._1) = newelems(rows._2)
    newelems(rows._2) = tmprow
    new Matrix(newelems)
  }

  override def toString: String = {
    ""
  }
}


