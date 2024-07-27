package net.degoes.zio

import net.degoes.zio.InterviewGeneric.{iterateAndCollect, questions}
import zio._

import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Looping extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement a `repeat` combinator using `flatMap` (or `zipRight`) and recursion.
   */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    n match {
      case 0 => ZIO.succeed(Chunk.empty[A])
      case x => effect
        .flatMap(res =>
          repeat(x - 1)(effect)
            .map(others => res +: others)
        )
    }

  val run =
    repeat(100)(Console.printLine("All work and no play makes Jack a dull boy"))
}

object Interview extends ZIOAppDefault {
  import java.io.IOException

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `getAllAnswers` function in such a fashion that it will ask
   * the user each question and collect them all into a list.
   */
  def getAllAnswers(questions: List[String]): ZIO[Any, IOException, List[String]] =
    questions match {
      case Nil     => ZIO.succeed(List.empty[String])
      case q :: qs => for {
        _ <- Console.printLine(q)
        res <- Console.readLine
        others <- getAllAnswers(qs)
      } yield res +: others
    }

  /**
   * EXERCISE
   *
   * Use the preceding `getAllAnswers` function, together with the predefined
   * `questions`, to ask the user a bunch of questions, and print the answers.
   */
  val run = getAllAnswers(questions)
    .flatMap(_.foldLeft(ZIO.attempt(()))((eff, str) =>
      eff.zipRight(Console.printLine(str).unit)))

}

object InterviewGeneric extends ZIOAppDefault {
  import java.io.IOException

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `iterateAndCollect` function.
   */
  def iterateAndCollect[R, E, A, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as match {
      case Nil     => ZIO.succeed(List.empty[B])
      case a :: as => f(a).flatMap(res => iterateAndCollect(as)(f).map(res +: _))
    }

  val run =
    iterateAndCollect(questions)(q => Console.printLine(q) *> Console.readLine)
      .flatMap(_.foldLeft(ZIO.attempt(()))((eff, str) =>
        eff.zipRight(Console.printLine(str).unit)))
}

object InterviewForeach extends ZIOAppDefault {

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Using `ZIO.foreach`, iterate over each question in `questions`, print the
   * question to the user (`Console.printLine`), read the answer from the user
   * (`Console.readLine`), and collect all answers into a collection. Finally, print
   * out the contents of the collection.
   */
  val run = for{
    answers <- iterateAndCollect(questions)(q => Console.printLine(q) *> Console.readLine)
    _ <- ZIO.foreachDiscard(answers)(Console.printLine(_))
  } yield ()
}

object WhileLoop extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement the functional effect version of a while loop so the
   * application runs correctly.
   */
  def whileLoop[R, E, A](cond: UIO[Boolean])(zio: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    cond.flatMap {
      case false => ZIO.succeed(Chunk.empty[A])
      case true => zio.flatMap(res => whileLoop(cond)(zio).map(res +: _))
    }

  val run = {
    def loop(variable: Ref[Int]) =
      whileLoop(variable.get.map(_ < 100)) {
        for {
          value <- variable.get
          _     <- Console.printLine(s"At iteration: ${value}")
          _     <- variable.update(_ + 1)
        } yield ()
      }

    (for {
      variable <- Ref.make(0)
      _        <- loop(variable)
    } yield 0)
  }
}

object Iterate extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement the `iterate` function such that it iterates until the condition
   * evaluates to false, returning the "last" value of type `A`.
   */
  def iterate[R, E, A](start: A)(cond: A => Boolean)(f: A => ZIO[R, E, A]): ZIO[R, E, A] =
    if (cond(start)) {
      f(start).flatMap(iterate(_)(cond)(f))
    } else {
      ZIO.succeed(start)
    }

  val run =
    iterate(0)(_ < 100) { i =>
      Console.printLine(s"At iteration: ${i}").as(i + 1)
    }
}

object TailRecursive extends ZIOAppDefault {
  trait Response
  trait Request {
    def returnResponse(response: Response): Task[Unit]
  }

  lazy val acceptRequest: Task[Request] = ZIO.attempt(new Request {
    def returnResponse(response: Response): Task[Unit] =
      ZIO.attempt(println(s"Returning response ${response}"))
  })

  def handleRequest(request: Request): Task[Response] = ZIO.attempt {
    println(s"Handling request ${request}")
    new Response {}
  }

  /**
   * EXERCISE
   *
   * Make this infinite loop (which represents a webserver) effectfully tail
   * recursive.
   */
  lazy val webserver: Task[Nothing] =
      (for {
        request <- acceptRequest
        response <- handleRequest(request)
        _ <- request.returnResponse(response)
      } yield ()) *> webserver


  val run =
    (for {
      fiber <- webserver.fork
      _     <- ZIO.sleep(100.millis)
      _     <- fiber.interrupt
    } yield ())
}
