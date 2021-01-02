package org.bobosergiusz.patternmatcher

import scala.io.Source
import scala.io.StdIn.readLine
import java.{util => ju}

// TODO: clean 
trait IO[A] { self =>
    def run: A
    def map[B](f: A => B) =
      new IO[B] { def run: B = f(self.run) }
    def flatMap[B](f: A => IO[B]) =
      new IO[B] { def run: B = f(self.run).run }
}
object IO {
    def ReadLine: IO[String] =
        new IO[String] { def run = readLine }
    def PrintLine(s: String): IO[Unit] =
        new IO[Unit] { def run = println(s) }
    def ReadFile(fp: String): IO[Option[String]] =
      new IO[Option[String]] { 
        val lines = Source.fromFile(fp).getLines
        def run = try {
          Some(lines.next)
        } catch {
          case e: ju.NoSuchElementException => None
        }
      }
    def PrintIterator(i: Iterator[String]): IO[Iterator[Unit]] =
      IO.sequence(i.map(PrintLine))
    def unit[A](a: => A) =
      new IO[A] { def run: A = a }
    def map2[A, B, C](a: IO[A], b: IO[B])(f: (A, B) => C) = for {
        aa <- a
        bb <- b
    } yield f(aa, bb)
    def sequence[A](l: Iterator[IO[A]]): IO[Iterator[A]] =
      l.foldLeft(unit(Iterator[A]()))((b, a) => map2(a, b)(Iterator(_) ++ _))
}