package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val result = chars.foldLeft(0)((n, c) => c match {
      case '(' => if (n < 0) -1 else n + 1
      case ')' => if (n <= 0) -1 else n - 1
      case _ => n
    })

    result == 0
  }

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx < until) {
        val (a, b) = chars(idx) match {
          case '(' => (arg1 + 1, arg2)
          case ')' => if (arg1 > 0) (arg1 - 1, arg2) else (0, arg2 + 1)
          case _ => (arg1, arg2)
        }

        traverse(idx + 1, until, a, b)
      } else
        (arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val t = (until + from) / 2
        val (a, b) = parallel(reduce(from, t), reduce(t, until))

        val a1 = if (a._1 > b._2) a._1 - b._2 else 0
        val a2 = if (b._2 > a._1) b._2 - a._1 else 0

        (a1 + b._1, a2 + a._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
