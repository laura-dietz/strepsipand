package edu.umass.ciir.strepsipand.unsuperv

import edu.umass.ciir.strepsi.{SeqTools, LogTools}
import edu.umass.ciir.strepsipand.{NamedScoredText, QueryExpander}

/**
 * User: dietz
 * Date: 1/15/14
 * Time: 12:30 PM
 */
object RelevanceModel extends QueryExpander {
  def buildRm(scoredText: Seq[NamedScoredText], topK: Int, queryId: String): Seq[(String, Double)] = {
    val scoredProbText = LogTools.normLogProbs(scoredText.map(_.score)).zip(scoredText.map(_.terms))
    val scoredTerms =
      for ((docProb, text) <- scoredProbText; term <- text) yield (term, docProb / text.length)
    val term2probList = SeqTools.groupByKey(scoredTerms)
    val term2Prob = SeqTools.aggregateMapList[String, Double, Double](term2probList, by = _.sum)
    SeqTools.topK(term2Prob.toSeq, topK).sortBy(-_._2)
  }


}
