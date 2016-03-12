import scalaz._

trait Group[T] extends Monoid[T]{
  def |-|(a:T,b: =>T):T
}

trait Ring[T] extends Group[T]{
  def |*|(a:T,b: =>T):T
}

trait Field[T] extends Ring[T]{
  def |/|(a:T,b: =>T):T
}

case class Vectors[G](elem:G)(implicit vg:Group[G]){
  def :*:[F](b:F)(implicit ff:Field[F]) = new :*:(b,elem)
}

case class :*:[F,G](f:F,g:G)(implicit vg:Group[G],ff:Field[F]) extends VectorSpace[F,G](f,g)

abstract class VectorSpace[F, G](f:F,v:G)(implicit vg:Group[G],ff:Field[F])
