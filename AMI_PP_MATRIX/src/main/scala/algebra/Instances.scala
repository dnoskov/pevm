package algebra

object Instances{
  import Structures._
  implicit val doubleField = new Field[Double]{
    def id = 0.0
    def unit = 1.0
    def append(a:Double, b: =>Double):Double = a + b
    def subtract(a:Double, b: =>Double):Double = a - b
    def multiply(a:Double, b: =>Double):Double = a * b
    def divide(a:Double, b: =>Double):Double = a / b
  }
  implicit val intRing = new Ring[Int]{
    def id = 0
    def unit = 1
    def append(a:Int, b: =>Int):Int = a + b
    def subtract(a:Int, b: =>Int):Int = a - b
    def multiply(a:Int, b: =>Int):Int = a * b
  }
}
