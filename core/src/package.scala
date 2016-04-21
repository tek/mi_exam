package tryp
package mi

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}

trait MITypes
{
  type Mat = DenseMatrix[Double]
  type Col = DenseVector[Double]
  type Row = Transpose[Col]

  val Mat = DenseMatrix
  val Col = DenseVector
  val Row = Transpose

  type Weights = Nel[Mat]
}

object `package`
extends MITypes
with cats.std.AllInstances
with Fu.ToFuOps
with Fu2.ToFu2Ops
with Sample.ToSampleOps
with monocle.function.NonEmptyVectorIndexLens
with ToMatOps
