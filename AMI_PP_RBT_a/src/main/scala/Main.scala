import java.awt.Paint
import javax.swing.JFrame

import org.apache.commons.collections15.Transformer

import edu.uci.ics.jung.algorithms.layout.ISOMLayout
import edu.uci.ics.jung.graph._
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, ModalGraphMouse}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization._

import scala.io.StdIn

object Main extends App {
  var tree = RBT.empty[Int]

  def show = tree.bfs{ _ match {
      case a:Full[Int] => println(a.value)
      case a => println(a)
    }
  }

  def draw = {
    val g = new DirectedSparseGraph[RBT[Int], String]()
    tree.inorder{
      case (a:Full[Int],b:Full[Int]) =>
        g.addVertex(a)
        g.addEdge(a.value.toString + " to " + b.value.toString, b, a)
      case _ =>
    }

    val vertexPaint: Transformer[RBT[Int],Paint]  = new Transformer[RBT[Int],Paint] {
      def transform(t: RBT[Int]): Paint = {
        if (t.color == Color.Black) java.awt.Color.BLACK
        else java.awt.Color.RED
      }
    }

    val vs = new VisualizationViewer(new DefaultVisualizationModel(new ISOMLayout(g)))

    vs.getRenderContext.setVertexLabelTransformer(new ToStringLabeller())
    vs.getRenderContext.setVertexFillPaintTransformer(vertexPaint)
    val graphMouse = new DefaultModalGraphMouse()
    graphMouse.setMode(ModalGraphMouse.Mode.PICKING)
    vs.setGraphMouse(graphMouse)
    val frame = new JFrame()
    frame.getContentPane().add(vs)
    frame.pack()
    frame.setVisible(true)
  }

  def run: Unit = StdIn.readLine( /*"Введите команду(help: выводит список команд)\n"*/ ).split(" ") match {
    case Array("add", x) =>
      tree = tree :+ x.toInt; run
    case Array("contains", x) =>
      println(tree.contains(x.toInt)); run
    case Array("remove", x) =>
      tree.remove(x.toInt); run
    case Array("height") =>
      println(tree.height); run
    case Array("show") =>
      show; println; run
    case Array("draw") =>
      draw; run
    case Array("help") =>
      println("add x: добавляет х в дерево\n" +
        "contains x: возвращает true если х содержится в дереве и false иначе\n" +
        "remove x: удаляет элемент х из дерева\n" +
        "height: возвращает высоту дерева\n" +
        "show: печатает дерево в консоль\n" +
        "draw: рисует дерево\n" +
        "exit: завершает программу"); run
    case Array("exit") => ()
    case Array("get") =>
      println(tree.toString); run
    case _ => println("Ошибка, неизвестная команда"); run
  }
  run

}
