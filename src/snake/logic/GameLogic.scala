package snake.logic

import engine.random.RandomGenerator
import snake.logic.GameLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {
  var state = initGameState()

  def gameOver: Boolean = {
    if(state.body contains state.head)  state = state.gameOver()
    return state.gameOverBool
  }

  def step(): Unit = {
    if(!state.gameOverBool) {
      state = state.moveSnake()
    }
    state = state.resetChangedDir()
  }

  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    if(d != state.direction.opposite) state = state.changeDir(d)
  }

  def initGameState() : GameState ={
    var gameState = GameState(dims = gridDims,
                              apple = null,
                              head = Point(2, 0),
                              body = List(Point(1, 0), Point(0, 0)),
                              growCount = 0,
                              direction = East(),
                              changedDir = false,
                              gameOverBool =  false)
    gameState = gameState.initSnake()
    return gameState
  }

  def getCellType(p : Point): CellType = state.cellTypeAt(p)
  case class GameState (
                         dims : Dimensions,
                         apple : Point,
                         head : Point,
                         body : List[Point],
                         growCount : Int,
                         direction: Direction,
                         changedDir : Boolean,
                         gameOverBool : Boolean){
    def cellTypeAt(p : Point) : CellType = {
      if(isHead(p)) SnakeHead(direction)
      else if(isBody(p)) SnakeBody(getColor(p))
      else if(isApple(p)) Apple()
      else Empty()
    }
    private def isApple(p : Point) : Boolean = (apple == p)
    private def isHead(p : Point) : Boolean = (head == p)
    private def isBody(p : Point) : Boolean = (body contains p)

    def moveSnake() : GameState = {
      val newHead = head.movePoint(direction).checkOverflow(dims)
      val (newBody, newGrowCount) = moveBody(newHead)
      val newApple =
        if(newHead == apple) placeApple(newBody, newHead)
        else apple
      return copy(apple = newApple, head = newHead, body = newBody, growCount = newGrowCount)
    }

    def initSnake() : GameState = {
      copy(apple = placeApple(body, head))
    }

    def gameOver() : GameState = copy(gameOverBool = true)

    def placeApple(newBody : List[Point], newHead : Point) : Point = {
      var spots : List[Point] = List[Point]()

      for (y <- 0 until dims.height; x <- 0 until dims.width) {
        if (!(newBody contains Point(x, y)) && newHead != Point(x, y)) spots = spots :+ Point(x, y)
      }
      val placedApple =
        if(spots.length > 0) spots(random.randomInt(spots.length))
        else null
      return placedApple
    }

    private def moveBody(newHead : Point) : (List[Point], Int) = {
      val newBody = head +: body
      var newGrowCount = growCount
      val finalBody =
        if(growCount == 0) newBody.dropRight(1)
        else {
          newGrowCount = growCount - 1
          newBody
        }

      if(newHead == apple) newGrowCount += 3
      return (finalBody, newGrowCount)
    }

    def getColor(p : Point) : Float = (1 / (body.length).toFloat * (body.indexOf(p)).toFloat)

    def resetChangedDir() : GameState = copy(changedDir = false)

    def changeDir(d : Direction) : GameState = {
      if(!changedDir) copy(direction = d, changedDir = true)
      else this
    }
  }
}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 2.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

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


