package io.adri.fpsnake

import zio.{Has, ZIO}

import java.awt.{Color, Graphics}

object Draw {
  val Size = 10

  def snake(s: Snake): ZIO[Has[Graphics], Nothing, Unit] = (for {
    g <- ZIO.service[Graphics]
    _ <- ZIO.effect(g.setColor(Color.RED))
    _ <- ZIO.effect {
      s.body.foreach { bodyPart =>
        g.drawRect(bodyPart.x * Size, bodyPart.y * Size, Size, Size)
      }
    }
  } yield ()).tapError(t => ZIO.debug(t.getMessage)).orDie

  def food(food: Box): ZIO[Has[Graphics], Nothing, Unit] = (for {
    g <- ZIO.service[Graphics]
    _ <- ZIO.effect(g.setColor(Color.RED))
    _ <- ZIO.effect {
        g.drawRect(food.x * Size, food.y * Size, Size, Size)
      }
  } yield ()).tapError(t => ZIO.debug(t.getMessage)).orDie

  def gameOver(score: Int): ZIO[Has[Graphics], Nothing, Unit] = (for {
    g <- ZIO.service[Graphics]
    _ <- ZIO.effect(g.setColor(Color.GREEN))
    _ <- ZIO.effect {
      g.drawString(s"Game over!", 150, 200)
      g.drawString(s"Score $score", 150, 220)
    }
  } yield ()).tapError(t => ZIO.debug(t.getMessage)).orDie

}
