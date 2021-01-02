package org.bobosergiusz.patternmatcher

sealed trait Result 

case class Matched(matches: IntegerSet) extends Result {
    def cons(m: Matched): Result = Matched(this.matches union m.matches)
}
case object Unmatched extends Result


class IntegerSet(l: List[(Int, Int)]) {
    val intervals: List[(Int, Int)] =
      l.sortBy(_._1).foldLeft(List[(Int, Int)]())((b, a) => add(b, a)).reverse
    def union(is: IntegerSet) = new IntegerSet(intervals ++ is.intervals)

    private def add(l: List[(Int, Int)], ii: (Int, Int)): List[(Int, Int)] =
      l match {
          case Nil => List(ii)
          case h::t => if (ii._1 <= h._2) (h._1, ii._2)::t
                       else ii::h::t
      }
}

object IntegerSet {
    def apply(l: List[(Int, Int)]): IntegerSet = new IntegerSet(l)
    def apply(): IntegerSet = apply(List())
}