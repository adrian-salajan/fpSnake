package io.adri.fpsnake

import io.adri.fpsnake.Direction._

case class World(snake: Snake, food: Box, size: Box, gameOver: Boolean = false) {
  def nextFoodIsValid(food: Box) =
    !snake.body.contains(food)

  def willBeBit(snake: Snake): Boolean =
    snake.direction match {
      case Up => snake.body.tail.contains(snake.head.up)
      case Down => snake.body.tail.contains(snake.head.down)
      case Left => snake.body.tail.contains(snake.head.left)
      case Right => snake.body.tail.contains(snake.head.right)
    }

  def advance(newDirection: Option[Direction], nextFood: Box): World = {
    val nextDirectionSnake = newDirection.fold(this.snake)(newDir =>
      (snake.direction, newDir) match {
        case (Up | Down, Left | Right) => snake.changeDirection(newDir)
        case (Left | Right, Up | Down) => snake.changeDirection(newDir)
        case _ => snake
      }
    )

    if (willBeBit(nextDirectionSnake)) this.copy(gameOver = true)
    else {
      val moved = this.copy(snake = nextDirectionSnake.move())
      if (moved.canEat)
        World(
          Snake(moved.snake.body.prepended(food), moved.snake.direction),
          nextFood,
          size)
      else moved
    }
  }


  def canEat: Boolean = snake.direction match {
    case Direction.Up if (food == snake.head) => true
    case Direction.Down if (food == snake.head) => true
    case Direction.Right if (food == snake.head) => true
    case Direction.Left if (food == snake.head) => true
    case _=> false
  }

}
object World {

  def init = World(Snake.startingSnake, Box(15,10), Box(40, 40))
}

sealed trait Direction
object Direction {
  final object Up extends Direction
  final object Down extends Direction
  final object Left extends Direction
  final object Right extends Direction
}
case class Snake(body: Vector[Box], direction: Direction) {
  val head = body.head

  def changeDirection(newDirection: Direction) = copy(direction = newDirection)

  def move(): Snake = {
    direction match {
      case Direction.Up =>
        Snake(body.init.prepended(body.head.up), direction)
      case Direction.Down =>
        Snake(body.init.prepended(body.head.down), direction)
      case Direction.Left =>
        Snake(body.init.prepended(body.head.left), direction)
      case Direction.Right =>
        Snake(body.init.prepended(body.head.right), direction)
    }
  }
}

object Snake {
  private val startingHead = Box(10,10)
  val startingSnake = new Snake(
    Vector(startingHead, startingHead.left, startingHead.left.left, startingHead.left.left.left),
    direction = Direction.Right
  )
}


case class Box(x: Int, y: Int) {
  def up = Box(x, y - 1)
  def down = Box(x, y + 1)
  def left = Box(x - 1 , y)
  def right = Box(x + 1 , y)
}
