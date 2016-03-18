import scala.collection.mutable
import scala.reflect.ClassTag
import scala.math._

/**
  * Created by noname on 10.03.16.
  */

object Matrix {

}


case class Matrix[T](e: Row[T]*)(implicit fractional: Fractional[T], tag: ClassTag[T]) extends Cloneable {
  import fractional._
  import tag._

  val rows = e.toArray

  val dim = rows.length

  //  def this(e: Array[Row[T]])(implicit fractional: Fractional[T], tag: ClassTag[T]) =
  //    this(e: _*)
  //
  //  def this(dim: Int)(implicit fractional: Fractional[T], tag: ClassTag[T])= {
  //    this(new Array[Row[T]](dim))
  //    for (i <- this.rows.indices) this.rows(i) = new Row(new Array[T](dim))
  //  }

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
        copy.rows(j) = copy.rows(j) - (normrow * copy.rows(j)(i))
      }
    }

    result * mult
  }

  def norm: T = rows.maxBy((r) => r.norm).norm

  private def apply(i: Int): Row[T] = rows(i)

  def apply(i: Int, j: Int): T = rows(i)(j)

  def apply(i: Int, j: Int, x: T) = {
    rows(i).elems(j) = x
  }

  private def swap(i: Int, j: Int): Unit = {
    val tmprow = rows(i)
    rows(i) = rows(j)
    rows(j) = tmprow
  }

  def isSymmetric: Boolean = {
    for (i <- 0 until dim-1; j <- i+1 until dim if this(i,j) != this(j,i)) false
    true
  }

  def isUT: Boolean = {
    for (i <- 1 until dim; j <- 0 until i if this(i,j) != zero) false
    true
  }

  def isLT: Boolean = {
    for (i <- 0 until dim-1; j <- i+1 until dim if this(i,j) != zero) false
    true
  }

  def ==(m: Matrix[T]): Boolean = {
    for (i <- 0 until dim; j <- 0 until dim if this(i,j) != m(i,j)) false
    true
  }

  def row(i: Int): Array[T] = rows(i).e.toArray

  def col(j: Int): Array[T] = (for (i <- 0 until dim) yield this(i,j)).toArray

  def *(m: Matrix[T]): Matrix[T] = {
    val result = new Matrix(new Array[Row[T]](dim): _*)
    for (i <- result.rows.indices) {
      result.rows(i) = Row(new Array[T](dim): _*)
      for (j <- result.rows(i).elems.indices)
        result.rows(i).elems(j) = row(i).zip(m.col(j)) map {case (x,y) => x * y} reduceLeft { (a,b) => a + b}
    }
    result
  }

  def pow(a: Int): Matrix[T] = {
    var result = this.clone

    def pow_rec(a: Int): Matrix[T] = {
      if (a == 0) id
      else {
        result = pow_rec(a/2)
        result = result * result
        if (a % 2 == 1) result *= this
        result
      }
    }

    pow_rec(a)
  }

  def map(f: (Int, Int, T) => T): Matrix[T] = {
    val result = new Matrix(new Array[Row[T]](dim): _*)
    for (i <- result.rows.indices) {
      result.rows(i) = Row(new Array[T](dim): _*)
      for (j <- result.rows(i).elems.indices)
        result.rows(i).elems(j) = f(i,j,this(i,j))
    }
    result
  }

  def id: Matrix[T] = {map {
    (i,j,x) => if (i==j) one else zero
  }}

  override def toString: String = {
    val result = (for (r <- rows) yield r.toString).mkString("\n")
    result
  }

  override def clone = Matrix(rows.clone: _*)
}


