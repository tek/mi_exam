package tryp
package mi
package kmeans
package unit

class RandomSpec
extends MSVCheckSpec[KMeansData, KMeans, KMeans, Col, KMeansLearnConf]
{
  override def numTests = 1

  override def trials = 1.some

  override def epsilon0 = 0.999d

  lazy val dataGen = KMeansGen.kmeans(3, 3, Range(folds * 5, folds * 10))
}

class PlottedRandomSpec
extends PlottedCheckSpec[KMeansData, KMeans, KMeans, Col, KMeansLearnConf]
{
  override def numTests = 1

  override def trials = 1.some

  override def epsilon0 = .1d

  lazy val dataGen = KMeansGen.kmeans(3, 3, Range(folds * 5, folds * 10))
}
