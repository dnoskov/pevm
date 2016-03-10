import scala.annotation.tailrec

abstract sealed class Color
case object Red extends Color
case object Black extends Color

abstract class RBT[A](implicit ordering:Ordering[A]) {
  import ordering._
  def color: Color
  def value: A
  def left: RBT[A]
  def right: RBT[A]
  def isEmpty: Boolean
  def insert(x: A)(implicit ordering:Ordering[A]): RBT[A] = {
    def insert(t: RBT[A]): RBT[A] =
      if (t.isEmpty) RBT.make[A](Red, x,Leaf[A](),Leaf[A]())
      else if (x < t.value) balance(t.color, t.value, insert(t.left), t.right)
      else if (x > t.value) balance(t.color, t.value, t.left, insert(t.right))
      else t

    def balance(c: Color, x: A, l: RBT[A], r: RBT[A]) = (c, l, r) match {
      case (Black, Branch(Red, y, Branch(Red, z, a, b), c), d) => RBT.make(Red, y, RBT.make(Black, z, a, b), RBT.make(Black, x, c, d))
      case (Black, Branch(Red, z, a, Branch(Red, y, b, c)), d) => RBT.make(Red, y, RBT.make(Black, z, a, b), RBT.make(Black, x, c, d))
      case (Black, a, Branch(Red, y, b, Branch(Red, z, c, d))) => RBT.make(Red, y, RBT.make(Black, x, a, b), RBT.make(Black, z, c, d))
      case (Black, a, Branch(Red, z, Branch(Red, y, b, c), d)) => RBT.make(Red, y, RBT.make(Black, x, a, b), RBT.make(Black, z, c, d))
      case _ => RBT.make(c, x, l, r)
    }

    def blacken(t: RBT[A]) = RBT.make(Black, t.value, t.left, t.right)

    blacken(insert(this))
  }

  def height: Int = {
    def height(n:RBT[A]): Int = if (n.isEmpty) 0
      else math.max(height(n.left), height(n.right)) + 1
    height(this)-1
  }
  def inorder[B](f:(RBT[A],RBT[A])=>B): Unit = {
    def inorder(cur:RBT[A],par:RBT[A]): Unit ={
      if(cur==null)return
      f(cur,if(par!=null)par else null)
      if(!cur.left.isEmpty)inorder(cur.left,cur)
      if(!cur.right.isEmpty)inorder(cur.right,cur)
    }
    inorder(this,null)
  }
  def bfs[B](f:A=>B): Unit ={
    var q = Seq(this)
    while(q.nonEmpty){
      if(q.head!=null&& !q.head.isEmpty)f(q.head.value)
      q = q match {
        case null::xs => xs
        case Seq(x) => Seq(x.left,x.right)
        case x::xs => (xs :+ x.left) :+ x.right
      }
    }
  }
  def contains(key:A)(implicit ordering:Ordering[A]): Boolean = {
    @tailrec
    def contains(key:A,n:RBT[A]): RBT[A] ={
      if(n==null) null else if(key==n.value) n else
      if(key>n.value) contains(key,n.right) else contains(key,n.left)
    }
    contains(key,this)!=null
  }
  def remove(key:A)(implicit ordering:Ordering[A]): Unit = ???

}

case class Branch[A](color: Color,value: A,left: RBT[A],right: RBT[A])(implicit ordering:Ordering[A]) extends RBT[A] {
  def isEmpty = false
}

case class Leaf[A](implicit ordering:Ordering[A]) extends RBT[A] {
  def color: Color = Black
  def value: Nothing = throw new NoSuchElementException("Нет его.")
  def left: RBT[A] = null
  def right: RBT[A] = null
  def isEmpty = true
}

object RBT {
  def empty[A](implicit ordering:Ordering[A]): RBT[A] = Leaf()

  def make[A](c: Color, x: A, l: RBT[A], r: RBT[A])(implicit ordering:Ordering[A]): RBT[A] =
    Branch(c, x, l, r)

  def apply[A](xs: A*)(implicit ordering:Ordering[A]): RBT[A] = {
    var r: RBT[A] = Leaf[A]()
    for (x <- xs) r = r.insert(x)
    r
  }
}
