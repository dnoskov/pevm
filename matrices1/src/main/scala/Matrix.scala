import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by noname on 10.03.16.
  */

object Matrix {

}


case class Matrix[T](e: Row[T]*)(implicit fractional: Fractional[T]) {
  import fractional._

  val elems = e.toArray

  val dim = elems.length

  def this(e: Array[Row[T]])(implicit fractional: Fractional[T], tag: ClassTag[T]) =
    this(e: _*)

  def det: T = {
    val copy = Matrix(elems.clone: _*)

    def getClosestNonZeroRowIndex(i: Int): Int = {
      var j = i
      while (j < dim && copy(j,i) == 0) j += 1
      if (j == dim) -1 else j
    }

    var result = one
    var mult = one
    for (i <- 0 until dim) {
      if (copy(i,i) == zero) {
        val k = getClosestNonZeroRowIndex(i)
        if (k < 0) return zero
        else {
          swap(i,k)
          mult = -mult
        }
      }
      result *= copy(i,i)
      val normrow = copy(i) / copy(i,i)
      for (j <- i+1 until dim) {
        copy.elems(j) = copy.elems(j) - (normrow * copy.elems(j)(i))
      }
    }

    result
  }

  def apply(i: Int): Row[T] = elems(i)

  def apply(i: Int, j: Int): T = elems(i)(j)

  def swap(i: Int, j: Int): Unit = {
    val tmprow = elems(i)
    elems(i) = elems(j)
    elems(j) = tmprow
  }

  override def toString: String = {
    val result = (for (r <- elems) yield r.toString).mkString("\n")
    result
  }
}


