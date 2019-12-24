/**
 * GenSeqTests.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class GenSeqTests extends FlatSpec {

  behavior of "FunctionsTest.genSeq"

  /*-----------------------------------------------------------------------------------------------------------------------
  SOURCE: TESTING SCALA IN INTELLIJ WITH SCALATEST
  LINK: https://docs.scala-lang.org/getting-started/intellij-track/testing-scala-in-intellij-with-scalatest.html#inner-main
  -----------------------------------------------------------------------------------------------------------------------*/

  it should "have size = n" in {
    for (n <- 0 to 1000){
      val test = Functions.genSeq(n)
      assert(test.size == n)
    }
  }

  it should "have every element be even" in {
    for (n <- 0 to 1000){
      val test = Functions.genSeq(n)
      for (elem <- test){
        assert(elem % 2 == 0)
      }
    }
  }
}
