package tryp
package mi
package mlp
package unit

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}

class InternalSpec
extends Spec
{
  def is = s2"""
  input coord $input
  output coord $output
  back prop $backProp
  """

  case class Dat(feature: Col, target: Double)
  extends TrainData

  val layers = Nel(3)

  val steps = 1

  def beta = 1.0

  def eta = 1.0

  def transfer = Identity

  def weights =
    new ManualWeights(Nel(
      DenseMatrix.create(3, 2, Array(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
      DenseMatrix.create(1, 3, Array(1.0, 0.0, 0.0))
    ))

  implicit lazy val conf =
    TrainConf(transfer, beta, eta, layers, steps, weights)

  val in1 = 0.7

  lazy val train = {
    val x = DenseVector(in1, 0.0)
    val d = Nel(Dat(x, 1.0))
    Train(d)
  }

  lazy val point = train.points.head

  lazy val forward = point.forwardprop(train.initialWeights)

  lazy val backward = point.backprop(forward, train.initialWeights)

  def input = {
    forward.in must_== Nel(Col(in1, 0.0), Col(in1, 0.0, 0.0), Col(in1))
  }

  def output = {
    forward.out must_== Nel(Col(in1, 0.0), Col(in1, 0.0, 0.0), Col(in1))
  }

  def backProp = {
    val deltas = backward
    1 === 1
  }
}

class BasicSpec
extends Spec
{
  def is = s2"""
  main $main
  """

  val layers = Nel(7, 5, 3)

  val steps = 1000

  def beta = 2.0

  def eta = 1.0

  lazy val transfer = new Logistic(beta)

  implicit lazy val conf =
    TrainConf(transfer, beta, eta, layers, steps, RandomWeights)

  def main = {
    val d = Iris.loadNel
    val t = Train(d)
    val w = t.run
    val test = d.tail(51)
    val pt = PointTrain(test)
    pr(test)
    val testClass = pt.forwardprop(w).output
    test.target must beCloseTo(testClass, 0.001)
  }

  // TODO
  // create simple spec for testing whether the values end up at the right
  // matrix coordinate
  // by specifying only single non-null initial values for input and weights
}
