package tryp
package mi
package mlp

case class MLPPredictor(config: MLPLearnConf)
extends Predictor[Weights, MLP, Double]
{
  def transfer = config.transfer

  lazy val creator = MLPModelCreator(config)

  def input[S: Sample](sample: S) = {
    if (config.bias) Col.vertcat(Col(1d), sample.feature)
    else sample.feature
  }

  def layer(state: MLP, w: Mat) = {
    val in = w * state.hidden.last.out
    val out = transfer.f(in)
    state.addLayer(in, out)
  }

  def apply[S: Sample](sample: S, weights: Weights)
  (implicit mc: MC[S])
  : Prediction[S, MLP, Double] = {
    val z = MLP.init(input(sample), transfer.deriv, weights)
    val pred = weights.foldLeft(z)(layer)
    Prediction(sample, pred, pred.output, mc.predictedClass(pred.output))
  }
}

case class MLPSV[S: Sample](data: S, state: MLP)
(implicit mc: MC[S])
extends SV[S, Double]
{
  def output = state.output

  override lazy val predictedClass = mc.predictedClass(output)

  def error(cost: Func2) = data.value.map(cost.f(_, output)).toValidatedNel
}

case class MLPValidator[S: Sample]
(data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends Validator[MLP]
{
  lazy val predictor = MLPPredictor(config)

  def sample(weights: Weights)(sample: S): SampleValidation = {
    val pred = predictor(sample, weights)
    MLPSV(sample, pred.model)
  }

  def impl(weights: Weights) = Val(data map sample(weights))

  def stats(weights: Weights) = impl(weights).stats(config.cost)

  def error(weights: Weights) = stats(weights) map (_.totalError)

  def run(model: MLP) = impl(model.weights)
}
