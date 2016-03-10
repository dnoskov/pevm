import scala.annotation.tailrec

class Treap[T <: Ordered[T]](implicit num:Numeric[T]){
  import num._
  var root:Node = null

  class Node(val key:T,val priority:Int, var left:Node = null,var right:Node = null)

  def merge(node1:Node,node2:Node):Node ={
    if(node1==null) node2 else
    if(node2==null) node1 else
    if(node1.priority>node2.priority){
      node1.right = merge(node1.right,node2)
      node1
    } else {
      node2.left = merge(node1,node2.left)
      node2
    }
  }

  def split(node:Node,key:T): (Node,Node) ={
    if(node==null) (null,null) else
    if(node.key<=key){
      val result = split(node.right, key)
      node.right = result._1
      (node,result._2)
    } else {
      val result = split(node.left,key)
      node.left = result._2
      (result._1,node)
    }
  }

  def insert(key:T,priority:Int=scala.util.Random.nextInt): Unit ={
    if(!contains(key)) {
      val (left, right) = split(root, key)
      root = merge(left, merge(new Node(key, priority), right))
    }
  }

  def contains(key:T): Boolean ={
    @tailrec
    def contains(key:T,n:Node): Node ={
      if(n==null) null else if(key==n.key) n else
      if(key>n.key) contains(key,n.right) else contains(key,n.left)
    }
    contains(key,root)!=null
  }

  def remove(key:T): Unit ={
    val (t1,t2) = split(root,key)
    split(t1,key-one)
    merge(t1,t2)
  }

  def height: Int = {
    def height(n:Node): Int ={
      if(n==null) 0
      else Integer.max(height(n.left),height(n.right))+1
    }
    height(root)-1
  }

  def bfs[B](f:Node=>B): Unit ={
    var q = Seq(root)
    while(q.nonEmpty){
      f(q.head)
      q = q match {
        case null::xs => xs
        case Seq(x) => Seq(x.left,x.right)
        case x::xs => (xs :+ x.left) :+ x.right
      }
    }
  }

  def inorder[B](f:(Node,Node)=>B): Unit = {
    def inorder(cur:Node,par:Node): Unit ={
      if(cur==null)return
      f(cur,par)
      inorder(cur.left,cur)
      inorder(cur.right,cur)
    }
    inorder(root,null)
  }

}
