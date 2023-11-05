open Shared
open Layer
open FeedForward_Shared
open MathJs.Matrix.Float

let feedBatchInputThroughDenseLayer = (layer, inputs) => {
  switch layer {
  | DenseLayer(record) => {
      let {weights, biases} = record
      let values = multiply(inputs, weights)->add(biases)
      record.output = Some(values)
      values
    }
  | _ => raise(UnexpectedLayer("Expecting DenseLayer", layer))
  }
}

let forwardBatch: (array<layer>, floatMatrix) => floatMatrix = (layers, inputs) =>
  Belt.Array.reduce(layers, inputs, (layerInputs, layer) => {
    switch layer {
    | DenseGraphInputLayer(_) => map(layerInputs, vector => feedInput(layer, vector))
    | DenseGraphLayer(_) => map(layerInputs, vector => feedPriorLayerOutput(layer, vector))
    | Activation(_) => map(layerInputs, vector => feedActivation(layer, vector))
    | LayerActivation(_) => map(layerInputs, vector => feedActivation(layer, vector))
    | DenseLayer(_) => feedBatchInputThroughDenseLayer(layer, layerInputs)
    }
  })