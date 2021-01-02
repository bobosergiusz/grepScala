package org.bobosergiusz.patternmatcher
// TODO: clean
import scala.io.Source

object Input {
  type Input[A] = Output => (A, Output)
  def unit[A](a: => A): Input[A] = (o: Output) => (a, o)
  def map[A, B](i: Input[A])(f: A => B): Input[B] =
    (o: Output) => {
      val (a, o2) = i(o)
      (f(a), o2)
    }
  def map2[A, B, C](ia: Input[A], ib: Input[B])(f: (A, B) => C) =
    (o: Output) => {
      val (a, o2) = ia(o)
      val (b, o3) = ib(o2)
      (f(a, b), o3)
    }
  def flatMap[A, B](i: Input[A])(f: A => Input[B]): Input[B] =
    (o: Output) => {
      val (a, o2) = i(o)
      f(a)(o2)
    }
  def sequence[A](l: Iterator[Input[A]]): Input[Iterator[A]] =
    l.foldLeft(unit(Iterator[A]()))((b, a) => map2(a, b)(Iterator(_) ++ _))

  class InputOps[A](r: Input[A]) {
    def map[B](f: A => B) = Input.map(r)(f)
    def flatMap[B](f: A => Input[B]) = Input.flatMap(r)(f)
  }
  implicit def toOps[A](r: Input[A]): InputOps[A] = new InputOps[A](r)
}