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
  val gameStack = initGameStack()

  def gameOver: Boolean = gameStack.gameOver

  def step(): Unit = gameStack.step()


  def setReverse(r: Boolean): Unit = gameStack.setReverse(r)

  def changeDir(d: Direction): Unit = gameStack.changeDir(d)

  def initGameStack() : GameStack = {
    var initGameState = GameState(dims = gridDims,
                                  apple = null,
                                  snake = List(Point(2, 0), Point(1, 0), Point(0, 0)),
                                  growCount = 0,
                                  currentDirection = East(),
                                  newDirection = East(),
                                  gameOverBool =  false,
                                  logic = this)
    initGameState = initGameState.initSnake()
    val initGameStack : GameStack = GameStack(initGameState)
    return initGameStack
  }

  def getCellType(p : Point): CellType = gameStack.cellTypeAt(p)

  case class GameStack (start : GameState) {//in separate file or does not matter?

    private var frames : SStack[GameState] = SStack[GameState](start)
    private var reverseActive : Boolean = false
    private def currentFrame : GameState = frames.top

    def gameOver : Boolean = currentFrame.gameOverBool

    def step() : Unit = {
      if(reverseActive && frames.size > 1) frames = frames.pop
      else if(!currentFrame.gameOverBool && !reverseActive)frames = frames.push(currentFrame.moveSnake())
    }

    def setReverse(r : Boolean) : Unit = {
      reverseActive = r
    }

    def cellTypeAt(p : Point) : CellType = currentFrame.cellTypeAt(p)

    def changeDir(d : Direction) : Unit = {
      if(d != currentFrame.currentDirection.opposite) {
        val replaceFrame = currentFrame
        frames = frames.pop
        frames = frames.push(replaceFrame.changeDir(d))
      }
    }

    }
  case class GameState (//in separate class??? or does not matter
                         dims : Dimensions,
                         apple : Point,//set of points needed for 0,5 or is 1D list enough
                         snake : List[Point],
                         growCount : Int,
                         currentDirection: Direction,
                         newDirection : Direction,
                         gameOverBool : Boolean,
                         logic : GameLogic){
    def cellTypeAt(p : Point) : CellType = {
      if(isHead(p)) SnakeHead(currentDirection)
      else if(isBody(p)) SnakeBody(getColor(p))
      else if(isApple(p)) Apple()
      else Empty()
    }
    private def isApple(p : Point) : Boolean = (apple == p)
    private def isHead(p : Point) : Boolean = (snake(0) == p)
    private def isBody(p : Point) : Boolean = (snake contains p)

    def moveSnake() : GameState = {
      val newHead = snake(0).movePoint(newDirection).checkOverflow(dims)
      val (newBody, newGrowCount) = moveBody(newHead)
      val newSnake = newHead +: newBody
      val newApple =
        if(newHead == apple) placeApple(newSnake)
        else apple
      if(newBody contains newHead) return gameOver()
      else return copy(apple = newApple, snake = newSnake, growCount = newGrowCount, currentDirection = newDirection)
    }

    def initSnake() : GameState = {
      copy(apple = placeApple(snake))
    }

    private def gameOver() : GameState = copy(gameOverBool = true)

    def placeApple(snake : List[Point]) : Point = {
      var spots : List[Point] = List[Point]()

      for(i <- dims.allPointsInside) {
        if(!(snake contains i)) spots = spots :+ i
      }
      val placedApple =
        if(spots.length > 0) spots(logic.random.randomInt(spots.length))
        else null
      return placedApple
    }

    private def moveBody(newHead : Point) : (List[Point], Int) = {
      val newBody = snake
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

    def getColor(p : Point) : Float = (1 / (snake.length-1).toFloat * (snake.indexOf(p)).toFloat)//fix this

    def changeDir(d : Direction) : GameState = copy(newDirection = d)
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


