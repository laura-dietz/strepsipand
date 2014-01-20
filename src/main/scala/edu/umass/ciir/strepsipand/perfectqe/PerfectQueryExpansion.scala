package edu.umass.ciir.strepsipand.perfectqe

import edu.umass.ciir.strepsipand.{NamedScoredText, QueryExpander}
import edu.umass.ciir.strepsirank.collector.{StrepsiReRanker, StrepsiFeatureCollector}
import ciir.umass.edu.learning.RANKER_TYPE
import ciir.umass.edu.metric.APScorer
import edu.umass.ciir.strepsi.SeqTools

/**
 * User: dietz
 * Date: 1/19/14
 * Time: 5:04 PM
 */
class PerfectQueryExpansion(mu: Double, q2annotations: Map[String, Set[String]], feedbackDocToTruthRewrite: (String) =>
  String) extends
QueryExpander {
  def buildRm(docs: Seq[NamedScoredText], topK: Int, queryId: String): Seq[(String, Double)] = {

    val annotations = q2annotations.get(queryId)
    val truths = docs.map(st => {
      val set = annotations.getOrElse(Set.empty)
      val figDocname = st.docname
      val docname = feedbackDocToTruthRewrite(figDocname)
      set.contains(docname)
    }).map(if (_) 1 else 0)


    val fc = new StrepsiFeatureCollector("")

    generateTermDocTablaux(docs, fc, truths, queryId)

    StrepsiFeatureCollector.save(fc, "q" + queryId + ".fc")

    def submitTrainScore(score: Double, msg: String) {
      println("score = " + score + " " + msg)
    }

    var weightVector: Seq[(String, Double)] = null
    def submitWeightVector(weightVectorSet: Seq[(String, Double)]) {
      weightVector = weightVectorSet
    }


    val reranker = new StrepsiReRanker(fc, RANKER_TYPE.COOR_ASCENT)
    val metricScorer = new APScorer()
    metricScorer.setRelDocCount(queryId, truths.count(_ > 0))
    reranker.trainTestSplit(Set(queryId), Some(fc.defaultFeatures), metricScorer = metricScorer,
      testQueries = None, modelfilename = Some
        ("perfectqe-q" + queryId), submitTrainScore = submitTrainScore, submitWeightVector = submitWeightVector)
    val expansionTerms = weightVector.sortBy(-_._2).take(topK)
    println("Perfect Expansion Terms " + expansionTerms)
    expansionTerms
  }


  def generateTermDocTablaux(docs: Seq[NamedScoredText],
                             fc: StrepsiFeatureCollector,
                             truth: Seq[Int],
                             queryId: String) {
    val docsTf = docs.map(doc => SeqTools.countMap(doc.terms))
    val docLens = docs.map(_.terms.length)
    val corpusTf = SeqTools.sumMaps(docsTf)
    val corpusLens = docLens.sum

    val termPool = corpusTf.keys.toIndexedSeq
    val defFeatures =
      for (term <- termPool) yield {
        term -> computeTermDocFeature(0, 0, corpusTf.getOrElse(term, 0), corpusLens, mu)
      }
    fc.addDefaultFeatures(defFeatures)

    for ((isRel, docno) <- truth.zipWithIndex) {
      fc.addEntry(queryId, docno + "", Some(isRel))
    }

    for (((tf, len), docno) <- docsTf.zip(docLens).zipWithIndex) {
      val features =
        for (term <- termPool) yield {
          term -> computeTermDocFeature(tf.getOrElse(term, 0), len, corpusTf.getOrElse(term, 0), corpusLens, mu)
        }
      fc.addFeatures(queryId, docno + "", features)
    }
  }

  def computeTermDocFeature(tf: Int, docLen: Int, ctf: Int, cLen: Int, mu: Double): Double = {
    math.log(tf + mu * ctf / cLen) - math.log(docLen + mu)
    //    tf
  }

}
