abstract class RBT[A](implicit ordering:Ordering[A]) {
  import ordering._
  import RBT._
  import Color._
  def color: Color

  def bfs[B](f: RBT[A] => B): Unit = {
    var q = Seq(this)
    while (q.nonEmpty) {
      f(q.head)
      q = q match {
        case null :: xs => xs
        case Seq(x:Full[A]) => Seq(x.l, x.r)
        case (x:Full[A]) :: xs => xs :+ x.l :+ x.r
        case _ :: xs => xs
      }
    }
  }

  def inorder[B](f:(RBT[A],RBT[A])=>B): Unit = {
    def inorder(cur:RBT[A],par:RBT[A]): Unit ={
      if(cur==null)return
      f(cur,par)
      cur match {
        case x:Full[A] =>
          inorder(x.l,x)
          inorder(x.r,x)
        case _ =>
      }
    }
    inorder(this,null)
  }

  def height: Int = {
    def height(n:RBT[A]): Int = n match {
      case Empty() => 0
      case n:Full[A] => Integer.max(height(n.l),height(n.r))+1
    }
    height(this)-1
  }

  def remove(x:A):RBT[A] = this.elements.filter(_ != x).foldLeft(RBT.empty[A])(_ :+ _)

  def contains(x: A): Boolean = this match {
    case Empty() => false
    case Full(_, y, l, r) =>
      if (x < y) l.contains(x)
      else if (x > y) r.contains(x)
      else true
  }

  def +:(a: A): RBT[A] = blacken(this ins a)

  def :+(a: A): RBT[A] = blacken(this ins a)

  private def ins(x: A): RBT[A] = this match {
    case Empty() => Full(Red, x, Empty(), Empty())
    case Full(c, y, l, r) =>
      if (x < y) Full(c, y, l.ins(x), r).balance()
      else if (x > y) Full(c, y, l, r.ins(x)).balance()
      else this
  }

  def balance(): RBT[A] = this match {
    case B(R(R(a, x, b), y, c), z, d) => R(B(a, x, b), y, B(c, z, d))
    case B(R(a, x, R(b, y, c)), z, d) => R(B(a, x, b), y, B(c, z, d))
    case B(a, x, R(R(b, y, c), z, d)) => R(B(a, x, b), y, B(c, z, d))
    case B(a, x, R(b, y, R(c, z, d))) => R(B(a, x, b), y, B(c, z, d))
    case _ => this
  }

  def elements: List[A] = {
    def aux(t: RBT[A], acc: List[A]): List[A] = t match {
      case Empty() => acc
      case Full(_, a, l, r) => aux(l, a :: aux(r, acc))
    }
    aux(this, List.empty[A])
  }

  override def toString: String = this match {
    case e: Empty[A] => "nil"
    case f: Full[A] => f.value.toString
  }

}
case class Empty[A]()(implicit ordering:Ordering[A]) extends RBT[A] { def color = Color.Black }
case class Full[A](color: Color.Color, value: A, l: RBT[A], r: RBT[A])(implicit ordering:Ordering[A]) extends RBT[A]

object Color extends Enumeration {
  type Color = Value
  val Red, Black = Value
}

object B {
  def apply[A](l: RBT[A], a: A, r: RBT[A])(implicit ordering:Ordering[A]): RBT[A] = Full(Color.Black, a, l, r)

  def unapply[A](t: RBT[A]): Option[(RBT[A], A, RBT[A])] = t match {
    case Full(Color.Black, a, l, r) => Some((l, a, r))
    case _ => None
  }
}

object R {
  def apply[A](l: RBT[A], a: A, r: RBT[A])(implicit ordering:Ordering[A]): RBT[A] = Full(Color.Red, a, l, r)

  def unapply[A](t: RBT[A]): Option[(RBT[A], A, RBT[A])] = t match {
    case Full(Color.Red, a, l, r) => Some((l, a, r))
    case _ => None
  }
}

object RBT {
  def empty[A](implicit ordering:Ordering[A]): RBT[A] = Empty[A]()

  private def blacken[A](t: RBT[A])(implicit ordering:Ordering[A]): RBT[A] = t match {
    case Empty() => t
    case tt @ Full(_, _, _, _) => tt.copy(color = Color.Black)
  }

}
