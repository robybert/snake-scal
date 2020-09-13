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
  var state = GameState(gridDims, placeFirstApple(), Point(2, 0), List(Point(1, 0), Point(0, 0)), this)
  var gameOverBool : Boolean = false
  var switch = true
  var growCount = 0
  var currDirection : Direction = East()
  var lastDirection : Direction = East()
  spots : List[Point]

  def gameOver: Boolean = {
    if(state.body contains state.head) gameOverBool = true
    return gameOverBool
  }

  def step(): Unit = {
    state = state.moveSnake()
    lastDirection = currDirection
    switch = true
  }

  def placeFirstApple() : Point = {
    for (y <- 0 until gridDims.height; x <- 0 until gridDims.width) {
      if (!(List(Point(1, 0), Point(0, 0)) contains Point(x, y)) && Point(2, 0) != Point(x, y)) spots = spots :+ Point(x, y)
    }
    val placedApple =
      if(spots.length > 0) spots(random.randomInt(spots.length))
      else null
    spots = List[Point]()
    return placedApple
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
  dims : Dimensions,
  apple : Point,
  head : Point,
  body : List[Point],
  logic: GameLogic){

  def cellTypeAt(p : Point) : CellType = {
    if(isHead(p)) SnakeHead(logic.currDirection)
    else if(isBody(p)) SnakeBody(1)//TODO fix float value
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
  def moveSnake() : GameState = {
    val newHead = 
      if(logic.currDirection == North()) movePoint(head, 0, -1)
      else if(logic.currDirection == East()) movePoint(head, 1, 0)
      else if(logic.currDirection == South()) movePoint(head, 0 , 1)
      else movePoint(head, -1, 0)
    val newBody = moveBody(newHead)
    val newApple =
      if(newHead == apple) placeApple(newBody, newHead)
      else apple
    return copy(apple = newApple, head = newHead, body = newBody)
  }

  private def placeApple(body : List[Point], head : Point) : Point = {
    for (y <- 0 until dims.height; x <- 0 until dims.width) {
      if (!(body contains Point(x, y)) && head != Point(x, y)) spots = spots :+ Point(x, y)
    }
    val placedApple =
      if(spots.length > 0) spots(logic.random.randomInt(spots.length))
      else null
    spots = List[Point]()
    return placedApple

  }

  private def moveBody(newHead : Point) : List[Point] = {
    val newBody = head +: body
    val finalBody =
      if(logic.growCount == 0) newBody.dropRight(1)
      else {
        logic.growCount -= 1
        newBody
      }

    if(newHead == apple) {
      logic.growCount += 3
    }
    return finalBody
  }

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 2.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller
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


