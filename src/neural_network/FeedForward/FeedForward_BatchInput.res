open Shared
open Layer
open FeedForward_Shared
open MathJs.Matrix.Float

let feedBatchInputThroughDenseMatrixLayer = (layerRecord, inputs) => {
  let {weights, biases} = layerRecord
  let values = multiply(inputs, weights)->add(biases)
  layerRecord.output = Some(values)
  values
}

let feedDenseLayer = (denseLayer, layerInputs) =>
  switch denseLayer {
  | DenseGraphInputLayer(_) => map(layerInputs, vector => feedInput(denseLayer, vector))
  | DenseGraphLayer(_) => map(layerInputs, vector => feedPriorLayerOutput(denseLayer, vector))
  | DenseMatrixLayer(layerRecord) => feedBatchInputThroughDenseMatrixLayer(layerRecord, layerInputs)
  }

let forwardBatch: (array<layer>, floatMatrix) => floatMatrix = (layers, inputs) =>
  Belt.Array.reduce(layers, inputs, (layerInputs, layer) => {
    switch layer {
    | Dense({implementation: denseLayer}) => feedDenseLayer(denseLayer, layerInputs)
    | Activation({implementation: activationLayer}) =>
      map(layerInputs, vector => feedActivation(activationLayer, vector))
    }
  })