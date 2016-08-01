package tryp
package mi

import algebra.Semigroup

trait WeightsInstances
{
  implicit def instance_Semigroup_Weights: Semigroup[Weights] =
    new Semigroup[Weights] {
      def combine(a: Weights, b: Weights) =
        a.fzipWith(b)(_ + _)
    }
}
