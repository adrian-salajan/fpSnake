package io.adri.fpsnake

import zio.clock.Clock
import zio.duration._
import zio.stream.ZStream
import zio.{ExitCode, IO, Promise, Queue, Ref, URIO, ZEnv, ZIO, ZQueue, blocking, random}

import java.awt.event.{WindowEvent, WindowListener}
import javax.swing.{JFrame, SwingUtilities, WindowConstants}

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
        .forever.tapError(e => ZIO.debug(e)) race quit.await

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
      .mapM { case (kp, _) =>
        for {
          w <- world.get
          nextFoodCandidate = (random.nextIntBounded(w.size.x) <&> random.nextIntBounded(w.size.x)).map {case (x, y) => Box(x, y)}
          nextFood <- nextFoodCandidate.repeatUntil(candidate => w.nextFoodIsValid(candidate))

          newWorld =  kp match {
            case None => w.advance(None, nextFood)
            case Some(keyPressed) =>
              keyPressed.char match {
                case 'w' => w.advance(Some(Direction.Up), nextFood)
                case 's' => w.advance(Some(Direction.Down), nextFood)
                case 'a' => w.advance(Some(Direction.Left), nextFood)
                case 'd' => w.advance(Some(Direction.Right), nextFood)
                case _ => w
            }
          }
          _ <- world.set(newWorld)
          _ <- if (newWorld.gameOver) ZIO.fail("Game over") else ZIO.unit
        } yield newWorld
      }
      .runDrain

  } yield ()


}
