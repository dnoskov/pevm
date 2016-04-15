package algebra

object Structures {
  trait Semigroup[T]{
    def append(a:T,b: =>T):T
  }

  final class SemigroupOps[F](val self:F)(implicit val F: Semigroup[F]){
    final def |+|(other: => F): F = F.append(self, other)
  }

  implicit def ToSemigroupOps[F](v: F)(implicit F0: Semigroup[F]) = new SemigroupOps[F](v)

  trait Group[T] extends Semigroup[T]{
    def id:T
    def subtract(a:T,b: =>T):T
  }

  final class GroupOps[F](val self:F)(implicit val F: Group[F]){
    final def |-|(other: => F): F = F.subtract(self, other)
  }

  implicit def ToGroupOps[F](v: F)(implicit F0: Group[F]) = new GroupOps[F](v)

  trait Ring[T] extends Group[T]{
    def unit:T
    def multiply(a:T,b: =>T):T
  }

  final class RingOps[F](val self:F)(implicit val F: Ring[F]){
    final def |*|(other: => F): F = F.multiply(self, other)
  }

  implicit def ToRingOps[F](v: F)(implicit F0: Ring[F]) = new RingOps[F](v)

  trait Field[T] extends Ring[T]{
    def divide(a:T,b: =>T):T
  }

  final class FieldOps[F](val self:F)(implicit val F: Field[F]){
    final def |/|(other: => F): F = F.divide(self, other)
  }

  implicit def ToFieldOps[F](v: F)(implicit F0: Field[F]) = new FieldOps[F](v)

  abstract class VectorSpace[F,T](implicit f:Field[F]) extends Group[T]{
    def |*|(f:F,g: =>T):T
  }
}
