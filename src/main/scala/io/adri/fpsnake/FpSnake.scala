package io.adri.fpsnake

import zio.clock.Clock
import zio.stream.ZStream
import zio.{ExitCode, Has, IO, Promise, Queue, Ref, UIO, URIO, ZEnv, ZIO, ZQueue, ZRef, blocking}
import zio.duration._

import java.awt.event.{MouseAdapter, MouseEvent, MouseListener, WindowEvent, WindowListener}
import java.awt.{Color, Graphics}
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

object  FpSnake extends zio.App {
  case class KeyPressed(char: Char)


  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    (for {
      _<- ZIO.debug("running FpSnake")
      keyPressEvents <- ZQueue.bounded[KeyPressed](32)
      quit <- Promise.make[Nothing, Unit]
      snake <- ZRef.make(Snake.startingSnake)
      world = World(
        snake = snake,
        time = ZStream.repeatEffect(ZIO.unit *> ZIO.sleep(300.milli)).provideLayer(Clock.live))
      _ <- blocking.blocking(IO.succeed(SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = gameWindows(world, keyPressEvents, quit)
      })))
      _<- gameLogic(keyPressEvents, world).forever race quit.await
      _<- ZIO.debug("exiting FpSnake")
    } yield ()).exitCode
  }

  def gameWindows(world: World, events: Queue[KeyPressed], allDone: Promise[Nothing, Unit]): Unit = {
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

    val gamePanel = new GamePanel(world,  events)


    frame.add(gamePanel)

    frame.pack()
    frame.setVisible(true)

  }

  def gameLogic(mouseDrage: Queue[KeyPressed], world: World): ZIO[Clock, Nothing, Unit] = for {
      //update world based on events
    _ <- (ZStream.fromEffect(mouseDrage.poll) zip world.time)
      .tap(_ => ZIO.debug("tick"))
      .tap { case (Some(kp), _) =>
        kp.char match {
          case 'w' => world.snake.update(_.advance(SnakeMovingDirection.Up))
          case 's' => world.snake.update(_.advance(SnakeMovingDirection.Down))
          case 'a' => world.snake.update(_.advance(SnakeMovingDirection.Left))
          case 'd' => world.snake.update(_.advance(SnakeMovingDirection.Right))
          case _ => IO.unit
        }
      case (None, _) => world.snake.update(s => s.advance())
      }
      .runDrain

  } yield ()





}
