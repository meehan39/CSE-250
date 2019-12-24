/**
 * Functions.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: brmeehan
 * Person#: 50226243
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.objects

object Functions {
  def genNum(n: Int): Int = {
    1001
  }

  def genSeq(n: Int): Seq[Int] = {
    ???
  }

  def funThree(n: Int): Int = {
    n
  }

  def compSum(n: Int): Long = {
    var sum = 0
    for (i <- 1 to n){
      sum += i
    }
    sum
  }
}
