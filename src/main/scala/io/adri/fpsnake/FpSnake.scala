package io.adri.fpsnake

import zio.ZIO.debug
import zio.clock.Clock
import zio.stream.ZStream
import zio.{ExitCode, Has, IO, Promise, Queue, Ref, UIO, URIO, ZEnv, ZIO, ZQueue, ZRef, blocking}
import zio.duration._
import zio.random.Random

import java.awt.event.{MouseAdapter, MouseEvent, MouseListener, WindowEvent, WindowListener}
import java.awt.{Color, Graphics}
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

object FpSnake extends zio.App {
  case class KeyPressed(char: Char)


  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    (for {
      _ <- ZIO.debug("running FpSnake")
      keyPressEvents <- ZQueue.bounded[KeyPressed](32)
      quit <- Promise.make[Nothing, Unit]
      renderTime = ZStream.repeatEffect(ZIO.unit *> ZIO.sleep(10.milli)).provideLayer(Clock.live)
      worldTime = ZStream.repeatEffect(ZIO.unit *> ZIO.sleep(100.milli)).provideLayer(Clock.live)
      world <- Ref.make(World.init)
      _ <- blocking.blocking(IO.succeed(SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = gameWindows(world, renderTime, keyPressEvents, quit)
      })))
      _ <- gameLogic(keyPressEvents, world, worldTime)
        .forever
        .tapError(over => ZIO.debug(over.getMessage)) race quit.await
      _ <- ZIO.debug("exiting FpSnake")
    } yield ()).exitCode
  }

  def gameWindows(world: Ref[World], time: ZStream[Any, Nothing, Unit], events: Queue[KeyPressed], allDone: Promise[Nothing, Unit]): Unit = {
    val frame = new JFrame()
    frame.setTitle("FpSnake")
    frame.setLocationRelativeTo(null)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.addWindowListener(new WindowListener {
      override def windowOpened(e: WindowEvent): Unit = ()

      override def windowClosing(e: WindowEvent): Unit = zio.Runtime.default.unsafeRun(allDone.succeed(()))

      override def windowClosed(e: WindowEvent): Unit = ()

      override def windowIconified(e: WindowEvent): Unit = ()

      override def windowDeiconified(e: WindowEvent): Unit = ()

      override def windowActivated(e: WindowEvent): Unit = ()

      override def windowDeactivated(e: WindowEvent): Unit = ()
    })

    val gamePanel = new GamePanel(world, time, events)


    frame.add(gamePanel)

    frame.pack()
    frame.setVisible(true)

  }

  def gameLogic(events: Queue[KeyPressed], world: Ref[World], time: ZStream[Any, Nothing, Unit]) = for {
    //update world based on events
    _ <- (ZStream.fromEffect(events.poll) zip time)
      .tap {
        case (Some(kp), _) =>
          kp.char match {
            case 'w' => world.get.flatMap(_.advance(Some(Direction.Up))).flatMap(world.set)
            case 's' => world.get.flatMap(_.advance(Some(Direction.Down))).flatMap(world.set)
            case 'a' => world.get.flatMap(_.advance(Some(Direction.Left))).flatMap(world.set)
            case 'd' => world.get.flatMap(_.advance(Some(Direction.Right))).flatMap(world.set)
            case _ => IO.unit
          }
        case (None, _) => world.get.flatMap(_.advance(None)).flatMap(world.set)
      }
      .runDrain

  } yield ()


}
