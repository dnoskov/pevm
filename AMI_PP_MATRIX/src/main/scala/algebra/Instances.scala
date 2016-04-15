package algebra

object Instances{
  import Structures._
  implicit val doubleAll = new Semigroup[Double] with Group[Double]
                                  with Ring[Double] with Field[Double]{
    def append(a:Double, b: =>Double):Double = a + b
    def subtract(a:Double, b: =>Double):Double = a - b
    def multiply(a:Double, b: =>Double):Double = a * b
    def divide(a:Double, b: =>Double):Double = a / b
  }
}
