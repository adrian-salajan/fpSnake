package io.adri.fpsnake

import zio.clock.Clock
import zio.random.Random
import zio.stream.ZStream
import zio.{Ref, UIO, ZIO, random}

case class World(snake: Snake, food: Box, size: Box) {

  def advance(newDirection: Option[Direction]) = {
    val moved = newDirection.fold(new World(snake.advance(), food, size))(
      direction => World(snake.advance(direction), food, size)
    )
    if (moved.canEat)
      generateFood.map(newFood => eat(newFood))
   else ZIO.succeed(moved)
  }

  def canEat: Boolean = (snake.direction, food) match {
    case (Direction.Up, f) if (f.x == snake.head.x && snake.head.y - 1 == food.y) => true
    case (Direction.Down, f) if (f.x == snake.head.x && snake.head.y + 1 == food.y) => true
    case (Direction.Right, f) if (f.y == snake.head.y && snake.head.x + 1 == food.x) => true
    case (Direction.Left, f) if (f.y == snake.head.y && snake.head.x - 1 == food.x) => true
    case _=> false
  }

  val generateFood: ZIO[Random, Nothing, Box] = (for {
    x <- random.nextIntBounded(size.x)
    y <- random.nextIntBounded(size.y)
  } yield Box(x, y)).repeatUntil(newFood => !snake.body.contains(newFood))

  def eat(newFood: Box) = World(Snake(snake.body.prepended(food), snake.direction), newFood, size)

}
object World {

  def init = World(Snake.startingSnake, Box(8,8), Box(50, 50))
}

sealed trait Direction
object Direction {
  final object Up extends Direction
  final object Down extends Direction
  final object Left extends Direction
  final object Right extends Direction
}
case class Snake(body: Vector[Box], direction: Direction) {
  def advance(newDirection: Direction = direction): Snake = {
    newDirection match {
      case Direction.Up =>
        Snake(body.init.prepended(body.head.up), newDirection)
      case Direction.Down =>
        Snake(body.init.prepended(body.head.down), newDirection)
      case Direction.Left =>
        Snake(body.init.prepended(body.head.left), newDirection)
      case Direction.Right =>
        Snake(body.init.prepended(body.head.right), newDirection)
    }
  }
  val head = body.head
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
