open Shared
open Layer
open FeedForward_Shared
open MathJs.Matrix.Float

let feedDenseLayer = (layerRecord, inputs) => {
  let {weights, biases} = layerRecord
  let values = inputs->fromVector->transpose->multiply(weights, _)->add(transpose(biases))
  layerRecord.output = Some(values)
  let valuesArray = toArrayOfVectors(values)
  let valuesVector = Belt.Array.getUnsafe(valuesArray, 0)

  valuesVector
}

let feedDenseLayer = (denseLayer, vector) =>
  switch denseLayer {
  | DenseGraphInputLayer(_) => feedInput(denseLayer, vector)
  | DenseGraphLayer(_) => feedPriorLayerOutput(denseLayer, vector)
  | DenseMatrixLayer(layerRecord) => feedDenseLayer(layerRecord, vector)
  }

let forwardInput: (array<layer>, floatVector) => floatVector = (layers, inputs) =>
  Belt.Array.reduce(layers, inputs, (layerInputs, layer) => {
    switch layer {
    | Dense({implementation: denseLayer}) => feedDenseLayer(denseLayer, layerInputs)
    | Activation({implementation: activationLayer}) => feedActivation(activationLayer, layerInputs)
    }
  })