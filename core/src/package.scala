package tryp
package mi

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}

import simulacrum._

trait MITypes
{
  type Mat = DenseMatrix[Double]
  type Col = DenseVector[Double]
  type Row = Transpose[Col]

  val Mat = DenseMatrix
  val Col = DenseVector
  val Row = Transpose

  type Weights = Nel[Mat]

  type ValiDouble = Validated[String, Double]
}

object `package`
extends MITypes
with cats.std.AllInstances
with Fu.ToFuOps
with Fu2.ToFu2Ops
with Sample.ToSampleOps
with ModelClasses.ToModelClassesOps
with ModelValue.ToModelValueOps
with monocle.function.NonEmptyVectorIndexLens
with ToMatOps
with ToBreezeFoldableOps
with ToBreezeSeqOps
with ModelState.ToModelStateOps
with PullFunctions
with StreamInstances
with ToXorStreamOps
with ToListStreamOps
with TaskInstances
with BreezeInstances
with WeightsInstances
{
  val pinf = Double.PositiveInfinity
}
