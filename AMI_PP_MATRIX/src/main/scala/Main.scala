import scalaz._
object Main extends App {
  // implicit val intsemigroup = new Semigroup[Int]{
  //   def append(a:Int):Int = a.+(this.asInstanceOf[Int])
  // }
  implicit val intgroup = new Group[Int]{
    def zero = 0
    def append(f1: Int, f2: => Int): Int = f1 + f2
    def |-|(a: Int,b: =>Int): Int = a - b
  }
  implicit val doublefield = new Field[Double]{
    def zero = 0
    def append(f1: Double, f2: => Double): Double = f1 + f2
    def |-|(a: Double,b: =>Double): Double = a - b
    def |*|(a: Double,b: =>Double): Double = a * b
    def |/|(a: Double,b: =>Double): Double = a / b
  }
  val v = new Vectors(5)
  val x = 6.0 :*: v
  x match {
    case a :*: b => println(a)
  }
}
