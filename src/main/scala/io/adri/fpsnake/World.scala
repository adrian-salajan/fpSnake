package io.adri.fpsnake

import zio.clock.Clock
import zio.stream.ZStream
import zio.{Ref, UIO, ZIO}

case class World(snake: Ref[Snake], time: ZStream[Any, Nothing, Unit])
object World {


}

sealed trait SnakeMovingDirection
object SnakeMovingDirection {
  final object Up extends SnakeMovingDirection
  final object Down extends SnakeMovingDirection
  final object Left extends SnakeMovingDirection
  final object Right extends SnakeMovingDirection
}
case class Snake(body: Vector[Box], direction: SnakeMovingDirection) {
  def advance(newDirection: SnakeMovingDirection = direction): Snake = {
    newDirection match {
      case SnakeMovingDirection.Up =>
        Snake(body.init.prepended(body.head.up), newDirection)
      case SnakeMovingDirection.Down =>
        Snake(body.init.prepended(body.head.down), newDirection)
      case SnakeMovingDirection.Left =>
        Snake(body.init.prepended(body.head.left), newDirection)
      case SnakeMovingDirection.Right =>
        Snake(body.init.prepended(body.head.right), newDirection)
    }
  }
  val head = body.head
}

object Snake {
  private val startingHead = Box(10,10)
  val startingSnake = new Snake(
    Vector(startingHead, startingHead.left, startingHead.left.left, startingHead.left.left.left),
    direction = SnakeMovingDirection.Right
  )
}


case class Box(x: Int, y: Int) {
  def up = Box(x, y - 1)
  def down = Box(x, y + 1)
  def left = Box(x - 1 , y)
  def right = Box(x + 1 , y)
}
