package snake.logic

// you can alter this file!

case class Point(x : Int, y : Int){
  def +(p : Point) : Point = Point(this.x + p.x, this.y + p.y)

  def movePoint(d : Direction): Point = this + d.toPoint

  def checkOverflow(dims : Dimensions) : Point =  {
    val p = Point(this.x % dims.width, this.y % dims.height)
    if(p.x < 0) p + Point(dims.width, 0)
    else if (p.y < 0) p + Point(0, dims.height)
    else p
  }
}
