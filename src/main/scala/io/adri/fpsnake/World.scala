package io.adri.fpsnake

import zio.clock.Clock
import zio.random.Random
import zio.stream.ZStream
import zio.{Ref, UIO, ZIO, random}
import Direction._
case class World(snake: Snake, food: Box, size: Box) {

  def isBit(snake: Snake): Boolean =
    snake.direction match {
      case Up => snake.body.tail.contains(snake.head)
      case Down => snake.body.tail.contains(snake.head)
      case Left => snake.body.tail.contains(snake.head)
      case Right => snake.body.tail.contains(snake.head)
    }

  def advance(newDirection: Option[Direction]) = {
    val moved = newDirection.fold(new World(snake.advance(), food, size))(
      newDir =>
        (snake.direction, newDir) match {
          case (Up | Down, Left | Right) => World(snake.advance(newDir), food, size)
          case (Left | Right, Up | Down) => World(snake.advance(newDir), food, size)
          case _ => World(snake.advance(), food, size)
        }

    )
    if (isBit(snake)) ZIO.fail(new RuntimeException(s"Game over, length was ${snake.body.size}"))
    else if (moved.canEat)
      generateFood.map(newFood => eat.copy(food = newFood))
   else ZIO.succeed(moved)
  }

  def canEat: Boolean = (snake.direction, food) match {
    case (Direction.Up, f) if (food == snake.head) => true
    case (Direction.Down, f) if (food == snake.head) => true
    case (Direction.Right, f) if (food == snake.head) => true
    case (Direction.Left, f) if (food == snake.head) => true
    case _=> false
  }

  val generateFood: ZIO[Random, Nothing, Box] = (for {
    x <- random.nextIntBounded(size.x)
    y <- random.nextIntBounded(size.y)
  } yield Box(x, y)).repeatUntil(newFood => !snake.body.contains(newFood))

  def eat = World(Snake(snake.body.prepended(food), snake.direction), food, size)

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
