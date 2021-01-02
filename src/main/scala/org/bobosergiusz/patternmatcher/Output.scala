package org.bobosergiusz.patternmatcher
// TODO: change names to something more meeningfull (these are pure functions not IO)
trait Output {
  def apply(in: String, r: Result): (String, Output)
}

class SimpleMarker(val m1: String, val m2: String) extends Output {
  def apply(in: String, r: Result): (String, Output) = r match {
    case Unmatched        => ("", this)
    case Matched(matches) => (mark(matches, in), this)
  }

  def mark(matches: IntegerSet, in: String): String = {
    val m1Len = m1.length
    val m2Len = m2.length
    matches.intervals
      .foldLeft((in, 0)) { case ((s, n), (i1, i2)) =>
        (
          s.patch(i1 + n * (m1Len + m2Len), m1, 0)
            .patch(i2 + n * (m1Len + m2Len) + m1Len, m2, 0),
          n + 1
        )
      }
      ._1 + "\n"
  }
}

import scala.collection.immutable.Queue
private class FiniteQueue[A] private (q: Queue[A], val size: Int) {
  def enqueue(elem: A): FiniteQueue[A] =
    if (q.length < size) new FiniteQueue(q.enqueue(elem), size)
    else {
      val (_, smaller) = q.dequeue
      new FiniteQueue(smaller.enqueue(elem), size)
    }
  override def toString = "Finite" + q.toString
  def foldLeft[B](z: B)(f: (B, A) => B) = q.foldLeft(z)(f)
}

private object FiniteQueue {
  def apply[A](size: Int) = new FiniteQueue[A](Queue[A](), size)
}
private class PastLines(fq: FiniteQueue[String]) {
  def collect = fq.foldLeft("")(_ + _)
  def clear = PastLines(fq.size)
  def add(s: String) = new PastLines(fq.enqueue(s + "\n"))
}

private object PastLines {
  def apply[A](size: Int) = new PastLines(FiniteQueue(size))
}

private class ContextedMarker(
    m1: String,
    m2: String,
    val past: PastLines,
    val futureToShow: Int,
    val maxFuture: Int
) extends SimpleMarker(m1, m2) {
  override def apply(in: String, r: Result): (String, Output) = r match {
    case Unmatched =>
      if (futureToShow > 0) emitUnmatched(in)
      else collectUnmatched(in)
    case Matched(matches) => (past.collect + mark(matches, in), matchedLast)
  }
  private def collectUnmatched(s: String): (String, ContextedMarker) = (
    "",
    new ContextedMarker(m1, m2, past.add(s), futureToShow, maxFuture)
  )
  private def emitUnmatched(s: String): (String, ContextedMarker) = (
    s + "\n",
    new ContextedMarker(m1, m2, past, futureToShow - 1, maxFuture)
  )

  private def matchedLast: ContextedMarker =
    new ContextedMarker(m1, m2, past.clear, maxFuture, maxFuture)
}

object Marker {
    def apply(m1: String, m2: String, p: Int, f: Int): Output =
      if (p == 0 && f == 0) new SimpleMarker(m1, m2)
      else new ContextedMarker(m1, m2, PastLines(p), 0, f)
}