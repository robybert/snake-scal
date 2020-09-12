package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {
  val state = GameState(gridDims, Point(1,1), Point(2, 0), List(Point(1, 0), Point(0, 0)), this)
  var gameOverBool : Boolean = false
  var switch = true
  var growCount = 0
  var currDirection : Direction = East()
  var lastDirection : Direction = East()
  spots : List[Point]

  def gameOver: Boolean = {
    if(gameOverBool) true//TODO fix
    else false
  }

  def step(): Unit = {
    state.moveSnake()
    lastDirection = currDirection
    switch = true
  }

  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    if(switch) {
      lastDirection = currDirection
      if (d.opposite != lastDirection) {
        d match {
          case North() => currDirection = North()
          case East() => currDirection = East()
          case South() => currDirection = South()
          case West() => currDirection = West()
        }
        switch = false
      }
    }
  }

  def getCellType(p : Point): CellType = state.cellTypeAt(p)
}

case class GameState (
  val dims : Dimensions,
  var apple : Point,
  var head : Point,
  var body : List[Point],
  logic: GameLogic){
  placeApple()

  def cellTypeAt(p : Point) : CellType = {
    if(isHead(p)) SnakeHead(logic.currDirection)
    else if(isBody(p)) SnakeBody(1)
    else if(isApple(p)) Apple()
    else Empty()
  }
  private def isApple(p : Point) : Boolean = (apple == p)
  private def isHead(p : Point) : Boolean = (head == p)
  private def isBody(p : Point) : Boolean = (body contains p)
  private def movePoint(p : Point, x : Int, y : Int) : Point = {
    if((p.x + x) < 0) Point(dims.width - 1, p.y)
    else if((p.x + x) >= dims.width) Point(0, p.y)
    else if((p.y + y) < 0) Point(p.x, dims.height - 1)
    else if((p.y + y) >= dims.height) Point(p.x, 0)
    else Point(p.x + x, p.y + y)
  }
  def moveSnake() : Unit = {
    if(logic.currDirection == North()) head = movePoint(head, 0, -1)
    else if(logic.currDirection == East()) head = movePoint(head, 1, 0)
    else if(logic.currDirection == South()) head = movePoint(head, 0 , 1)
    else head = movePoint(head, -1, 0)
    moveBody(logic.currDirection)
  }

  private def placeApple() : Unit = {
    for (y <- 0 until dims.height; x <- 0 until dims.width) {
      if (!(body contains Point(x, y)) && head != Point(x, y)) spots = spots :+ Point(x, y)
    }
    if(spots.length > 0) {
      apple = spots(logic.random.randomInt(spots.length))
      spots = List[Point]()
    }
  }

  private def moveBody(d : Direction) : Unit = {//TODO update name
    if(d == North()) {
      if(logic.lastDirection == East()) body = movePoint(body.head, 1, 0) +: body
      else if(logic.lastDirection == West()) body = movePoint(body.head, -1, 0) +: body
      else body = movePoint(body.head, 0, -1) +: body

    }
    else if(d == East()) {
      if(logic.lastDirection == North()) body = movePoint(body.head, 0, -1) +: body
      else if(logic.lastDirection == South()) body = movePoint(body.head, 0, 1) +: body
      else body = movePoint(body.head, 1, 0) +: body
    }
    else if(d == South()) {
      if(logic.lastDirection == East()) body = movePoint(body.head, 1, 0) +: body
      else if(logic.lastDirection == West()) body = movePoint(body.head, -1, 0) +: body
      else body = movePoint(body.head, 0,1) +: body
    }
    else {
      if(logic.lastDirection == North()) body = movePoint(body.head, 0, -1) +: body
      else if(logic.lastDirection == South()) body = movePoint(body.head, 0, 1) +: body
      else body = movePoint(body.head, -1, 0) +: body
    }
    if(body.init.contains(head) && logic.growCount == 0) logic.gameOverBool = true
    else if(body.contains(head) && logic.growCount != 0) logic.gameOverBool = true
    else if(head != apple && logic.growCount == 0) body = body.dropRight(1)
    else if(head == apple) {
      if(logic.growCount == 0) body = body.dropRight(1)
      else logic.growCount -= 1
      logic.growCount += 3
      placeApple()
    }
    else logic.growCount -= 1
  }

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 2.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller
//  var currDirection : Direction = East()
//  var lastDirection : Direction = East()
//  var growCount : Int  = 0
//  var gameOver : Boolean = false
//  var switch : Boolean = true
    var spots : List[Point] = List[Point]()

    // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
    : Dimensions =
    Dimensions(width = 10, height = 10)  // you can adjust these values to play on a different sized board
}


