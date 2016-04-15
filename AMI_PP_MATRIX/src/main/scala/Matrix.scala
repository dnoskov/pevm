import algebra.Structures._
import algebra.Instances._

object Matrix {
  implicit val doubleMatrixRing = new Ring[Matrix[Double]] {
    def id = ???
    def unit = ???
    def append(a: Matrix[Double], b: => Matrix[Double]): Matrix[Double] = {
      val n = a().length
      val na = new Array[Array[Double]](n)
      for (i <- 0 until n) {
        na(i) = new Array[Double](n)
        for (j <- 0 until n) na(i)(j) = a(i)(j) + b(i)(j)
      }
      Matrix(na)
    }
    def subtract(a: Matrix[Double], b: => Matrix[Double]): Matrix[Double] = {
      val n = a().length
      val na = new Array[Array[Double]](n)
      for (i <- 0 until n) {
        na(i) = new Array[Double](n)
        for (j <- 0 until n) na(i)(j) = a(i)(j) - b(i)(j)
      }
      Matrix(na)
    }
    def multiply(a: Matrix[Double], b: => Matrix[Double]): Matrix[Double] = {
      val na = new Array[Array[Double]](a().length)
      for (i <- 0 until a().length) {
        na(i) = new Array[Double](b().length)
        for (j <- 0 until b().length)
          for (k <- 0 until a(0).length)
            na(i)(j) += a(i)(k) + b(k)(j)
      }
      Matrix(na)
    }
  }

  def |*|(a: Double, b: => Matrix[Double]): Matrix[Double] = Matrix(b().map(_.map(_ * a)))

  def transpose(a: Matrix[Double]) = Matrix(a().transpose)

  def isSquare(a: Matrix[Double]) = a().length == a(0).length

  def isDiag(a: Matrix[Double]) = a == transpose(a)

  def isZero(a: Matrix[Double]) = a == zero(a().length)

  def zero(n: Int) = Matrix(Array.fill(n)(Array.fill(n)(0.0)))

  def id(n: Int):Matrix[Double] = {
    val na = Array.fill(n)(Array.fill(n)(0.0))
    for (i <- 0 until n) na(i)(i) = 1.0
    Matrix(na)
  }
  def isId(a: Matrix[Double]) = {
     (a |-| id(a().length)) == zero(a().length)
   }
}

case class Matrix[T](elems: Array[Array[T]])(implicit f: Field[T], m: Manifest[Array[Array[Double]]]) {
  def apply() = elems
  def apply(i: Int) = elems(i)
  override def toString() = elems.map(x => x.mkString("(", ", ", ")")).mkString("\n")
}
