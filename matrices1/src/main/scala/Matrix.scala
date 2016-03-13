import scala.collection.mutable
import scala.reflect.ClassTag
import scala.math._

/**
  * Created by noname on 10.03.16.
  */

object Matrix {

}


case class Matrix[T](e: Row[T]*)(implicit fractional: Fractional[T]) extends Cloneable {
  import fractional._

  val elems = e.toArray

  val dim = elems.length

  def this(e: Array[Row[T]])(implicit fractional: Fractional[T], tag: ClassTag[T]) =
    this(e: _*)

  def this(dim: Int) = {
    this(new Array[Row[T]](dim))
    for (i <- this.elems.indices) this.elems(i) = new Row[T](dim)
  }

  def det: T = {
    val copy = this.clone

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

  def norm: T = elems.maxBy((r) => r.norm).norm

  private def apply(i: Int): Row[T] = elems(i)

  def apply(i: Int, j: Int): T = elems(i)(j)

  private def swap(i: Int, j: Int): Unit = {
    val tmprow = elems(i)
    elems(i) = elems(j)
    elems(j) = tmprow
  }

  def isSymmetric: Boolean = {
    for (i <- 0 until dim-1; j <- i+1 until dim if (this(i,j) != this(j,i))) return false
    return true
  }

  def isUT: Boolean = {
    for (i <- 1 until dim; j <- i-1 until dim-1 if (this(i,j) != zero)) return false
    return true
  }

  def isLT: Boolean = {
    for (i <- 0 until dim-1; j <- i+1 until dim if (this(i,j) != zero)) return false
    return true
  }

  def ==(m: Matrix[T]): Boolean = {
    for (i <- 0 until dim; j <- 0 until dim if (this(i,j) != m(i,j))) return false
    return true
  }

  def row(i: Int): Array[T] = elems(i).elems.toArray

  def col(j: Int): Array[T] = (for (i <- 0 until dim) yield this(i,j)).toArray

  def *(m: Matrix[T]): Matrix[T] = {
    var result = new Matrix[T](dim)
    for (i <- 0 until dim; j <- 0 until dim)
      result.elems(i) elems j = 
  }

  override def toString: String = {
    val result = (for (r <- elems) yield r.toString).mkString("\n")
    result
  }

  override def clone = Matrix(elems.clone: _*)
}


