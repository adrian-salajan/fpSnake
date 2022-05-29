package io.adri.fpsnake

import io.adri.fpsnake.FpSnake.KeyPressed
import zio.stream.ZStream
import zio.{Has, Queue, RIO, Ref, UIO, ZIO}

import java.awt.event.{KeyAdapter, KeyEvent, KeyListener, MouseAdapter, MouseEvent}
import java.awt.{Color, Dimension, Graphics, Graphics2D}
import javax.swing.JPanel

class GamePanel(world: Ref[World], time: ZStream[Any, Nothing, Unit], events: Queue[KeyPressed]) extends JPanel {
  override def getPreferredSize: Dimension = new Dimension(500, 500)

  override def isFocusable: Boolean = true


  addMouseListener(new MouseAdapter {
    override def mouseReleased(e: MouseEvent): Unit = zio.Runtime.default.unsafeRun {
      events.offer(KeyPressed('p'))
    }
  })

  zio.Runtime.default
    .unsafeRunAsync_(time.tap(_ =>ZIO.succeed(this.repaint())).runDrain)


  addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit =
      zio.Runtime.default.unsafeRun(
        ZIO.debug(e.getKeyChar) *>
          events.offer(KeyPressed(e.getKeyChar)) *> ZIO.succeed(repaint())
      )
  })


  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    setBackground(Color.BLACK)
    zio.Runtime.default.unsafeRun {
      (for {
        s <- world.get.map(_.snake)
        food <- world.get.map(_.food)
        _ <- Draw.food(food)
        _ <- Draw.snake(s)
      } yield ()
        ).provide(Has(g))
    }

  }


}
