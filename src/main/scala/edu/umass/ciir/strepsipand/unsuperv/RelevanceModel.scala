package edu.umass.ciir.strepsipand.unsuperv

import edu.umass.ciir.strepsi.{SeqTools, LogTools}

/**
 * User: dietz
 * Date: 1/15/14
 * Time: 12:30 PM
 */
object RelevanceModel {
  def buildRm(scoredText: Seq[(Double, Seq[String], String)], topK: Int): Seq[(String, Double)] = {
    val scoredProbText = LogTools.normLogProbs(scoredText.map(_._1)).zip(scoredText.map(_._2))
    val scoredTerms =
      for ((docProb, text) <- scoredProbText; term <- text) yield (term, docProb / text.length)
    val term2probList = SeqTools.groupByKey(scoredTerms)
    val term2Prob = SeqTools.aggregateMapList[String, Double, Double](term2probList, by = _.sum)
    SeqTools.topK(term2Prob.toSeq, topK).sortBy(-_._2)
  }

  def renormalize(languageModel: Seq[(String, Double)]): Seq[(String, Double)] = {
    val normalizer = languageModel.map(_._2).sum
    for ((term, weight) <- languageModel) yield term -> weight / normalizer
  }

}
