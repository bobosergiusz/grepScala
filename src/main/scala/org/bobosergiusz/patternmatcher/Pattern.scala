package org.bobosergiusz.patternmatcher

abstract class Pattern {
    self =>
    def matches(in: String): Result

    def unary_! : Pattern = new Pattern {
        def matches(in: String): Result = self.matches(in) match {
            case Matched(_) => Unmatched
            case Unmatched=> Matched(IntegerSet())
        }
    }
    def or(p: Pattern): Pattern = new Pattern {
        def matches(in: String): Result = (self.matches(in), p.matches(in)) match {
            case (Unmatched, Unmatched) => Unmatched
            case (m1@Matched(_), m2@Matched(_)) => m1 cons m2
            case (m1@Matched(_), Unmatched) => m1
            case (Unmatched, m2@Matched(_)) => m2
        }
    }
    def and(p: Pattern): Pattern = new Pattern {
        def matches(in: String): Result = (self.matches(in), p.matches(in)) match {
            case (Unmatched, Unmatched) => Unmatched
            case (m1@Matched(_), m2@Matched(_)) => m1 cons m2
            case (m1@Matched(_), Unmatched) => Unmatched
            case (Unmatched, m2@Matched(_)) => Unmatched
        }
    }
}

object Pattern {
    def regex(p: String): Pattern = new Pattern {
        def matches(in: String): Result = {
            val l = (for (m <- p.r findAllMatchIn in)
            yield (m.start, m.end)).toList
            if (l.isEmpty) Unmatched
            else Matched(IntegerSet(l))
        }
    }
}