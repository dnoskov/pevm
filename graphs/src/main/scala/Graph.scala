import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by noname on 10.03.16.
  */
class Graph(v: ArrayBuffer[ArrayBuffer[Int]]) {

  def BFS[B](root: Int, visit: (Int) => B): ArrayBuffer[B] = {
    var q = ArrayBuffer[Int](root)
    var greys = mutable.Set[Int](root)
    var result = ArrayBuffer.empty[B]
    while (q.nonEmpty) {
      for (i <- v(q.head)) {
        if (!(greys contains i)) {
          greys += i
          q += i
        }
      }
      result += visit(q.head)
      q = q.tail
    }
    return result
  }

  def DFS[B](root: Int, f: (Int) => B): ArrayBuffer[B] = {

    var q = ArrayBuffer[Int](root)
    var greys = mutable.Set[Int](root)
    var result = ArrayBuffer.empty[B]

    def dfs(u: Int): Unit = {
      greys += u

      for (w <- v(u) if !(greys contains w)) dfs(w)
      result += f(u)
    }

    dfs(root)

    return result
  }

}