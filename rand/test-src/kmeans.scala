package tryp
package mi
package kmeans
package unit

class RandomSpec
extends MSVCheckSpec[KMeansData, KMeans, KMeans, Col, KMeansLearnConf]
{
  override def numTests = 1

  override def trials = 1.some

  override def epsilon = 10d * genData.sampleRange

  lazy val dataGen = KMeansGen.kmeans(3, 3, Range(folds * 5, folds * 10))
}

class PlottedRandomSpec
extends PlottedCheckSpec[KMeansData, KMeans, KMeans, Col, KMeansLearnConf]
{
  override def numTests = 1

  override def trials = 1.some

  lazy val dataGen = KMeansGen.kmeans(3, 3, Range(folds * 5, folds * 10))
}
