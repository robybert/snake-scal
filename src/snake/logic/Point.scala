package snake.logic

// you can alter this file!

case class Point(x : Int, y : Int){
  def +(p : Point) : Point = Point(this.x + p.x, this.y + p.y)

  def movePoint(d : Direction): Point = this + d.toPoint

  def checkOverflow(dims : Dimensions) : Point = Point((this.x + dims.width) % dims.width, (this.y + dims.height) % dims.height)
}
