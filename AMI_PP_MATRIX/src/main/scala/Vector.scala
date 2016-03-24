object Vector{
  import AlgebraicStructures._
  type Vector = Array[Double]
  implicit val vectorVS = new VectorSpace[Double,Vector] with Group[Vector] {
    def |+|(a:Vector,b: =>Vector):Vector = (for(i <- 0 until a.length) yield (a(i) + b(i))).toArray
    def |-|(a:Vector,b: =>Vector):Vector = (for(i <- 0 until a.length) yield (a(i) - b(i))).toArray
    def |*|(f:Double,b: =>Vector):Vector = b.map(_ * f)
  }
}
