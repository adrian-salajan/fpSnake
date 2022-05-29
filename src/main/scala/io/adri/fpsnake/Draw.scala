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
        println(bodyPart.x * Size)
        g.drawRect(bodyPart.x * Size, bodyPart.y * Size, Size, Size)
      }
    }
  } yield ()).tapError(t => ZIO.debug(t.getMessage)).orDie

}
