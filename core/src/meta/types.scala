package tryp
package mi
package meta

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}

trait MITypes
{
  type Mat = DenseMatrix[Double]
  type Col = DenseVector[Double]
  type Row = Transpose[Col]

  val Mat = DenseMatrix
  val Col = DenseVector
  val Row = Transpose
}

trait MLPTypes
{
  type Weights = Nel[Mat]
  type Layers = Nel[Col]
}
