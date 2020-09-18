package snake.logic

case class GameState(
                      dims: Dimensions,
                      apple: Point,
                      snake: List[Point],
                      growCount: Int,
                      currentDirection: Direction,
                      newDirection: Direction,
                      gameOverBool: Boolean,
                      logic: GameLogic) {

  def cellTypeAt(p: Point): CellType = {
    if (isHead(p)) SnakeHead(currentDirection)
    else if (isBody(p)) SnakeBody(getColor(p))
    else if (isApple(p)) Apple()
    else Empty()
  }

  private def isApple(p: Point): Boolean = (apple == p)

  private def isHead(p: Point): Boolean = (snake(0) == p)

  private def isBody(p: Point): Boolean = (snake contains p)

  def getColor(p: Point): Float = (1 / (snake.length - 1).toFloat * (snake.indexOf(p)).toFloat) //fix this

  def moveSnake(): GameState = {
    val newHead = snake(0).movePoint(newDirection).checkOverflow(dims)
    val (newBody, newGrowCount) = moveBody(newHead)
    val newSnake = newHead +: newBody
    val newApple =
      if (newHead == apple) placeApple(newSnake)
      else apple
    if (newBody contains newHead) return gameOver()
    else return copy(apple = newApple, snake = newSnake, growCount = newGrowCount, currentDirection = newDirection)
  }

  private def gameOver(): GameState = copy(gameOverBool = true)

  private def moveBody(newHead: Point): (List[Point], Int) = {
    val newBody = snake
    var newGrowCount = growCount
    val finalBody =
      if (growCount == 0) newBody.dropRight(1)
      else {
        newGrowCount = growCount - 1
        newBody
      }

    if (newHead == apple) newGrowCount += 3
    return (finalBody, newGrowCount)
  }

  def initSnake(): GameState = {
    copy(apple = placeApple(snake))
  }

  def placeApple(snake: List[Point]): Point = {
    var spots: List[Point] = List[Point]()

    for (i <- dims.allPointsInside) {
      if (!(snake contains i)) spots = spots :+ i
    }
    val placedApple =
      if (spots.length > 0) spots(logic.random.randomInt(spots.length))
      else null
    return placedApple
  }

  def changeDir(d: Direction): GameState = copy(newDirection = d)
}
