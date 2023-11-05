open Shared
open Neuron
open Synapse

type graphImplementation = {
  neurons: array<neuron>,
  mutable initialWeights?: floatMatrix,
  mutable inputSynapses?: array<synapse>,
  mutable outputSynapses?: array<synapse>,
}

type matrixImplementation = {
  @live numInputs: int,
  @live numNeurons: int,
  mutable weights: floatMatrix,
  mutable biases: floatMatrix,
  mutable output?: floatMatrix,
}

type denseLayer =
  | DenseMatrixLayer(matrixImplementation)
  | DenseGraphLayer(graphImplementation)
  | DenseGraphInputLayer(graphImplementation)

type activationRecord = {fn: float => float}
type layerActivationRecord = {fn: floatVector => floatVector}

type activationLayer =
  | Neuron(activationRecord)
  | Layer(layerActivationRecord)

type layer = Dense({implementation: denseLayer}) | Activation({implementation: activationLayer})

exception UnexpectedLayer(string, layer)
exception UnexpectedDenseLayer(string, denseLayer)
exception LayerMismatch(string, layer, layer)

let raiseNotDense = layer => raise(UnexpectedLayer("Not dense Layer", layer))
let raiseNoSynapses = layer => raise(UnexpectedDenseLayer("No synapses in Layer", layer))
let raiseNoNeurons = layer => raise(UnexpectedDenseLayer("No neurons in Layer", layer))
let raiseInvalidLayerConnection = (layer1, layer2) =>
  raise(LayerMismatch("Layers cannot be connected", layer1, layer2))
let raiseExpectedWeightedSynapses = (layer1, layer2) =>
  raise(LayerMismatch("Expected layers to share weighted synapse", layer1, layer2))

let makeDenseGraphLayer = (~weights: option<floatMatrix>=?, neurons) =>
  switch weights {
  | Some(w) => Dense({implementation: DenseGraphLayer({neurons, initialWeights: w})})
  | _ => Dense({implementation: DenseGraphLayer({neurons: neurons})})
  }

let makeDenseGraphInputLayer = (weights: option<floatMatrix>, neurons) =>
  switch weights {
  | Some(w) => Dense({implementation: DenseGraphInputLayer({neurons, initialWeights: w})})
  | _ => Dense({implementation: DenseGraphInputLayer({neurons: neurons})})
  }

let makeDenseMatrixLayer = (nInputs, nNeurons) => {
  open MathJs.Matrix.Float
  Dense({
    implementation: DenseMatrixLayer({
      numInputs: nInputs,
      numNeurons: nNeurons,
      weights: initRandom([nInputs, nNeurons], -1., 1.),
      biases: zeros([1, nNeurons]),
    }),
  })
}

let makeReLU = () => Activation({
  implementation: Neuron({fn: x => MathJs.Statistics.Float.max(x, 0.)}),
})

let makeSoftmax = () => {
  let sumArray = xs => MathJs.Vector.Float.reduce(xs, 0.0, (a, b) => a +. b)
  let calcProb = (xs, total) => MathJs.Vector.Float.map(xs, x => x /. total)
  let softmax = xs => {
    let e_xs = xs->MathJs.Vector.Float.map(x => MathJs.General.pow(MathJs.General.e, x))
    let total = sumArray(e_xs)
    calcProb(e_xs, total)
  }
  Activation({
    implementation: Layer({fn: softmax}),
  })
}

let getDenseLayer = layer =>
  switch layer {
  | Dense({implementation}) => implementation
  | _ => raiseNotDense(layer)
  }

let getNeurons = layer => {
  switch layer {
  | DenseGraphInputLayer({neurons, _}) => neurons
  | DenseGraphLayer({neurons, _}) => neurons
  | _ => raiseNoNeurons(layer)
  }
}

let getInputSynapses = layer => {
  let inputSynapses = switch layer {
  | DenseGraphLayer(record) => record.inputSynapses
  | _ => raiseNoSynapses(layer)
  }
  switch inputSynapses {
  | Some(inputSynapses) => inputSynapses
  | _ => []
  }
}

let connectLayer1ToLayer2 = (layer1, layer2) => {
  let connectLayers = (layerRecord1, layerRecord2) => {
    let {neurons: neurons1, ?initialWeights, _} = layerRecord1
    let {neurons: neurons2, ?inputSynapses} = layerRecord2
    switch (initialWeights, inputSynapses) {
    | (_, Some(_)) => ignore()
    | (None, None) => raiseExpectedWeightedSynapses(layer1, layer2)
    | (Some(w), None) => {
        let synapses = makeSynapses(neurons1, w, neurons2)
        layerRecord1.outputSynapses = Some(synapses)
        layerRecord2.inputSynapses = Some(synapses)
      }
    }
  }

  switch (layer1, layer2) {
  | (
      Dense({implementation: DenseGraphInputLayer(layerRecord1)}),
      Dense({implementation: DenseGraphLayer(layerRecord2)}),
    ) =>
    connectLayers(layerRecord1, layerRecord2)
  | (
      Dense({implementation: DenseGraphLayer(layerRecord1)}),
      Dense({implementation: DenseGraphLayer(layerRecord2)}),
    ) =>
    connectLayers(layerRecord1, layerRecord2)
  | (_, _) => raiseInvalidLayerConnection(layer1, layer2)
  }
  (layer1, layer2)
}

let connectLayers = (layers: array<layer>) => {
  let second = ((_, b)) => b
  let maybeConnect = (maybeL1, l2) =>
    switch maybeL1 {
    | None => Some(l2)
    | Some(l1) => connectLayer1ToLayer2(l1, l2)->second->Some
    }

  let connectMany = layers => Belt.Array.reduce(layers, None, maybeConnect)

  switch layers {
  | [] => ignore()
  | [_] => ignore()
  | [l1, l2] => connectLayer1ToLayer2(l1, l2)->ignore
  | _ => connectMany(layers)->ignore
  }
  layers
}