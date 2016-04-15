import algebra.Structures._

object Matrix {
  implicit def matrixRing[T](implicit F: Field[T], m2: Manifest[T]) = new Ring[Matrix[T]] {

    def id = ???

    def unit = ???

    def append(a: Matrix[T], b: => Matrix[T]): Matrix[T] = {
      val n = a().length
      val na = Array.fill(n)(Array.fill(n)(F.id))
      for (i <- 0 until n) {
        for (j <- 0 until n) na(i)(j) = a(i)(j) |+| b(i)(j)
      }
      Matrix(na)
    }

    def subtract(a: Matrix[T], b: => Matrix[T]): Matrix[T] = {
      val n = a().length
      val na = Array.fill(n)(Array.fill(n)(F.id))
      for (i <- 0 until n) {
        for (j <- 0 until n) na(i)(j) = a(i)(j) |-| b(i)(j)
      }
      Matrix(na)
    }

    def multiply(a: Matrix[T], b: => Matrix[T]): Matrix[T] = {
      val n = a().length
      val na = Array.fill(n)(Array.fill(n)(F.id))
      for (i <- 0 until a().length) {
        for (j <- 0 until b().length)
          for (k <- 0 until a(0).length)
            na(i)(j) = na(i)(j) |+| a(i)(k) |+| b(k)(j)
      }
      Matrix(na)
    }
  }

  def transpose[T](a: Matrix[T])(implicit F: Field[T], m1: Manifest[Array[T]]) = Matrix(a().transpose)

  def isDiag[T](a: Matrix[T])(implicit F: Field[T], m1: Manifest[T]) = a == transpose(a)

  def isZero[T](a: Matrix[T])(implicit F: Field[T], m1: Manifest[T]) = a == zero(a().length)

  def zero[T](n: Int)(implicit F: Field[T], m1: Manifest[T]) = Matrix(Array.fill(n)(Array.fill(n)(F.id)))

  def isSquare[T](a: Matrix[T])(implicit F: Field[T], m1: Manifest[T]) = a().length == a(0).length

  def id[T](n: Int)(implicit F: Field[T], m1: Manifest[T]): Matrix[T] = {
    val na = Array.fill(n)(Array.fill(n)(F.id))
    for (i <- 0 until n) na(i)(i) = F.unit
    Matrix(na)
  }

  def isId[T](a: Matrix[T])(implicit F: Field[T], m1: Manifest[T]) = {
    (a |-| id(a().length)) == zero(a().length)
  }
}

case class Matrix[T](elems: Array[Array[T]])(implicit f: Field[T], m: Manifest[Array[Array[T]]]) {

  def apply() = elems

  def apply(i: Int) = elems(i)

  override def toString() = elems.map(x => x.mkString("(", ", ", ")")).mkString("\n")
}
