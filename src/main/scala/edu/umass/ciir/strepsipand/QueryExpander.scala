package edu.umass.ciir.strepsipand

import scala._

/**
 * User: dietz
 * Date: 1/19/14
 * Time: 5:02 PM
 */
trait QueryExpander {
  def buildRm(scoredText: Seq[NamedScoredText], topK: Int, queryId: String): Seq[(String, Double)]

  def renormalize(languageModel: Seq[(String, Double)]): Seq[(String, Double)] = {
    val normalizer = languageModel.map(_._2).sum
    for ((term, weight) <- languageModel) yield term -> weight / normalizer
  }

}

case class NamedScoredText(score: Double, terms: Seq[String], docname: String)
