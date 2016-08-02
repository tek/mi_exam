package tryp
package mi
package rbf

trait RBFTypes
{
  type MC[S] = ModelClasses[S, Double]
}

object `package`
extends RBFTypes
with BasisFunction.ToBasisFunctionOps
