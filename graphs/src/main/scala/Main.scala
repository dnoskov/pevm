import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by noname on 10.03.16.
  */
object Main extends App {
  var g = ArrayBuffer.empty[ArrayBuffer[Int]]
  val src = Source.fromFile("test2")

  for (line <- src.getLines()) {
    g += ArrayBuffer.empty[Int]
    val tokens = line.split("\\s+")
    for (token <- tokens.tail) g.last += token.toInt
  }

  def f(x: Int): Int = x

  val graph = new Graph(g)
  println(graph.BFS(0, f))
  println(graph.DFS(0, f))


}
