package io.adri.fpsnake

import io.adri.fpsnake.FpSnake.KeyPressed
import zio.stream.ZStream
import zio.{Has, Queue, Ref, ZIO}

import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.{Color, Dimension, Graphics}
import javax.swing.JPanel

class GamePanel(world: Ref[World], renderTime: ZStream[Any, Nothing, Unit], events: Queue[KeyPressed]) extends JPanel {
  override def getPreferredSize: Dimension = new Dimension(400, 400)

  override def isFocusable: Boolean = true


  addMouseListener(new MouseAdapter {
    override def mouseReleased(e: MouseEvent): Unit = zio.Runtime.default.unsafeRun {
      events.offer(KeyPressed('p'))
    }
  })

  zio.Runtime.default
    .unsafeRunAsync_(renderTime.tap(_ => ZIO.succeed(this.repaint())).runDrain)


  addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit =
      zio.Runtime.default.unsafeRun(
        events.offer(KeyPressed(e.getKeyChar)) *> ZIO.succeed(repaint())
      )
  })


  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    setBackground(Color.BLACK)
    zio.Runtime.default.unsafeRun {
      (for {
        w <- world.get

        _ <- Draw.food(w.food)
        _ <- Draw.snake(w.snake)
        _ <- Draw.gameOver(w.snake.body.length).when(w.gameOver)
      } yield ()
        ).provide(Has(g))
    }

  }


}
