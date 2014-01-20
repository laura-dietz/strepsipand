package edu.umass.ciir.strepsipand.unsuperv

import junit.framework.{Assert, TestCase}
import edu.umass.ciir.strepsipand.NamedScoredText


/**
 * User: dietz
 * Date: 1/15/14
 * Time: 12:50 PM
 */
class TestRelevanceModel extends TestCase {
  val scoredText = Seq(
    NamedScoredText(-0.5, Seq("the", "fish", "stink", "stink"), "1"),
    NamedScoredText(-2.0, Seq("my", "fish", "smells"), "2"),
    NamedScoredText(-4.0, Seq("our", "dog", "stink"), "3"),
    NamedScoredText(-4.0, Seq("the", "dog", "stink"), "4")
  )

  def testRMFull() {
    println("Full")
    val lm = RelevanceModel.buildRm(scoredText, 10, "Q")
    println(lm.mkString("\n"))

    Assert.assertEquals("stink", lm.head._1)
    Assert.assertEquals(0.40523684270529314, lm.head._2)

    Assert.assertEquals("fish", lm(1)._1)
    Assert.assertEquals(0.2527233505385819, lm(1)._2)

  }

  def testRMOnly3TermsFull() {
    println("Full, 3 terms")
    val lm = RelevanceModel.buildRm(scoredText, 3, "Q")
    println(lm.mkString("\n"))
  }

  def testRMTop1() {
    println("Top1 doc")
    val lm = RelevanceModel.buildRm(scoredText.take(1), 10, "Q")
    println(lm.mkString("\n"))
  }

  def testRMTop2() {
    println("Top2 doc")
    val lm = RelevanceModel.buildRm(scoredText.take(2), 10, "Q")
    println(lm.mkString("\n"))
  }

  def testRMFullTwice() {
    println("Full twice")
    val lm = RelevanceModel.buildRm(scoredText ++ scoredText, 10, "Q")
    println(lm.mkString("\n"))
  }

  def testRMTop2Renormalized() {
    println("Top2 doc renormalized")
    val lm = RelevanceModel.renormalize(RelevanceModel.buildRm(scoredText.take(2), 10, "Q"))
    println(lm.mkString("\n"))
    Assert.assertEquals("stink", lm.head._1)
    Assert.assertEquals(0.4087872380968218, lm.head._2)
  }

}
