object AlgebraicStructures {
  implicit val doubleAll = new Semigroup[Double] with Group[Double]
                                  with Ring[Double] with Field[Double]{
    def |+|(a:Double, b: =>Double):Double = a + b
    def |-|(a:Double, b: =>Double):Double = a - b
    def |*|(a:Double, b: =>Double):Double = a * b
    def |/|(a:Double, b: =>Double):Double = a / b
  }
}

trait Semigroup[T]{
  def |+|(a:T,b: =>T):T
}

trait Group[T] extends Semigroup[T]{
  def |-|(a:T,b: =>T):T
}

trait Ring[T] extends Group[T]{
  def |*|(a:T,b: =>T):T
}

trait Field[T] extends Ring[T]{
  def |/|(a:T,b: =>T):T
}

abstract class VectorSpace[F,T](implicit f:Field[F]) extends Group[T]{
  def |*|(f:F,g: =>T):T
}
