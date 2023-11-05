open Shared
open Layer
open FeedForward_Shared
open MathJs.Matrix.Float

let feedDenseLayer = (layer, inputs) => {
  switch layer {
  | DenseLayer(record) => {
      let {weights, biases} = record
      let values = inputs->fromVector->transpose->multiply(weights, _)->add(transpose(biases))
      record.output = Some(values)
      let valuesArray = toArrayOfVectors(values)
      let valuesVector = Belt.Array.getUnsafe(valuesArray, 0)

      valuesVector
    }

  | _ => raise(UnexpectedLayer("Expecting DenseLayer", layer))
  }
}

let forwardInput: (array<layer>, floatVector) => floatVector = (layers, inputs) =>
  Belt.Array.reduce(layers, inputs, (layerInputs, layer) => {
    switch layer {
    | DenseGraphInputLayer(_) => feedInput(layer, layerInputs)
    | DenseGraphLayer(_) => feedPriorLayerOutput(layer, layerInputs)
    | DenseLayer(_) => feedDenseLayer(layer, layerInputs)
    | Activation(_) => feedActivation(layer, layerInputs)
    | LayerActivation(_) => feedActivation(layer, layerInputs)
    }
  })