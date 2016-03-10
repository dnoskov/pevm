import javax.swing.JFrame

import edu.uci.ics.jung.algorithms.layout.ISOMLayout
import edu.uci.ics.jung.graph.DirectedSparseGraph
import edu.uci.ics.jung.visualization.control.{DefaultModalGraphMouse, ModalGraphMouse}
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.{DefaultVisualizationModel, VisualizationViewer}

import scala.io.StdIn

object Main extends App {
  var tree = RBT.empty[Int]

  def show = tree.bfs{ _ match {
      case a:Full[Int] => println(a.value)
      case a => println(a)
    }
  }

  def draw = {
    val g = new DirectedSparseGraph[Int, String]()
    tree.inorder{
      case (a:Full[Int],b:Full[Int]) =>
        g.addVertex(a.value)
        g.addEdge(a.value.toString + " to " + b.value.toString, b.value, a.value)
      case _ =>
    }

    val vs = new VisualizationViewer(new DefaultVisualizationModel(new ISOMLayout(g)))

    vs.getRenderContext().setVertexLabelTransformer(new ToStringLabeller())
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
