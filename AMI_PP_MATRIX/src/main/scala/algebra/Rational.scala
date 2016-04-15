package algebra

import Structures._

object Rational{
  implicit def ring2rational[T](x:T)(implicit T0:Ring[T]):Rational[T] = new Rational(x, T0.unit)
  implicit def rationalField[T](implicit R:Ring[T]) = new Field[Rational[T]]{
    def id:Rational[T] = new /:/(R.id, R.unit)
    def unit:Rational[T] = new /:/(R.unit, R.unit)
    def append(a: Rational[T],b: => Rational[T]): Rational[T] = {
      val p/:/q = a
      val x/:/y = b
      ((p|*|y)|+|(x|*|q))/:/(y |*| q)
    }
    def subtract(a: Rational[T],b: => Rational[T]): Rational[T] = {
      val p/:/q = a
      val x/:/y = b
      ((p|*|y)|-|(x|*|q))/:/(y |*| q)
    }
    def multiply(a: Rational[T],b: => Rational[T]): Rational[T] = {
      val p/:/q = a
      val x/:/y = b
      (p|*|x)/:/(q|*|y)
    }
    def divide(a: Rational[T],b: => Rational[T]): Rational[T] = {
      val p/:/q = a
      val x/:/y = b
      (p|*|y)/:/(q|*|x)
    }
  }
}

class Rational[T](p:T,q:T)(implicit R: Ring[T]){
  def /:/(x:T):Rational[T] = new /:/(p,q |*| x)
  override def toString() = p+"/:/"+q
}

case class /:/[T](p:T,q:T)(implicit F0: Ring[T]) extends Rational[T](p,q)
