open Shared
open Neuron
open Layer
open Synapse
open MathJs.Matrix.Float
open MathJs.Vector.Float

//exception NotImplemented(string, layer)
//let raiseForwardMatrixNotImplemented = layer =>
//  raise(NotImplemented("forwardMatrix is not implemented for this Layer type", layer))

// todo: handle |inputs| != |layer.neurons|
let forwardFromInput = (layer, inputs) => {
  let get = MathJs.Vector.Float.get
  let toVector = MathJs.Vector.Float.toVector

  // map over each neuron in the layer
  let neurons = getNeurons(layer)
  let values = Belt.Array.mapWithIndex(neurons, (i, neuron) => {
    let neuronRecord = getInputNeuronRecord(neuron)
    // todo: this calculation is right in this place, but seems poor design
    // get the i'th input from the input vector, and apply the neurons function
    let value = neuronRecord.fn(get(inputs, i))

    // set the value of the neuron
    neuronRecord.value = Some(value)

    // return the value
    value
  })
  // convert array of values to vector of values
  toVector(values)
}

let petitionSynapsesByOutputNeuron = (outputNeuronToMatch, synapses) =>
  Belt.Array.partition(synapses, anotherSynapse => {
    let outputNeuron = getOutputNeuron(anotherSynapse)
    getNeuronId(outputNeuron) == getNeuronId(outputNeuronToMatch)
  })

let forwardFromPriorLayer2 = (layer, inputs) => {
  let neurons = getNeurons(layer)
  let synapses = getSynapses(layer)
  let values = Belt.Array.map(neurons, neuron => {
    let neuronRecord = getChainedNeuronRecord(neuron)

    // get weights from synapses
    let (matchedSynapses, _) = petitionSynapsesByOutputNeuron(neuron, synapses)
    let weights = Belt.Array.map(matchedSynapses, ((_, weight, _)) => weight)->toVector

    // get the bias (default 0)
    let bias = default(neuronRecord.bias, 0.)

    // todo: this calculation is right in this place, but seems poor design
    // get the i'th input from the input vector, and apply the neurons function
    let value = neuronRecord.fn(weights, inputs) +. bias

    // set the value of the neuron
    neuronRecord.value = Some(value)

    // return the value
    value
  })
  // convert array of values to vector of values
  toVector(values)
}

let forwardMatrixThroughMatrixLayer = (layer, inputs) => {
  switch layer {
  | LinearMatrixLayer(record) => {
      let {weights, biases} = record
      let weightsT = weights->transpose
      let values = multiply(inputs, weightsT)->add(biases)
      record.values = Some(values)
      values
    }
  | _ => raise(UnexpectedLayer("Expecting LinearMatrixLayer", layer))
  }
}

let forwardVectorThroughMatrixLayer = (layer, inputs) => {
  switch layer {
  | LinearMatrixLayer(record) => {
      let {weights, biases} = record
      let values = inputs->fromVector->transpose->multiply(weights, _)->add(transpose(biases))
      record.values = Some(values)
      let valuesArray = toArrayOfVectors(values)
      let valuesVector = Belt.Array.getUnsafe(valuesArray, 0)

      valuesVector
    }

  | _ => raise(UnexpectedLayer("Expecting LinearMatrixLayer", layer))
  }
}

let forwardVector: (array<layer>, floatVector) => floatVector = (layers, inputs) =>
  Belt.Array.reduce(layers, inputs, (layerInputs, layer) => {
    switch layer {
    | LinearInputLayer(_) => forwardFromInput(layer, layerInputs)
    | LinearLayer(_) => forwardFromPriorLayer2(layer, layerInputs)
    | LinearMatrixLayer(_) => forwardVectorThroughMatrixLayer(layer, layerInputs)
    }
  })

let forwardMatrix: (array<layer>, floatMatrix) => floatMatrix = (layers, inputs) =>
  Belt.Array.reduce(layers, inputs, (layerInputs, layer) => {
    switch layer {
    | LinearInputLayer(_) => {
        let matrixOutputs = map(layerInputs, vector => forwardFromInput(layer, vector))
        matrixOutputs
      }
    | LinearLayer(_) => {
        let matrixOutputs = map(layerInputs, vector => forwardFromPriorLayer2(layer, vector))
        matrixOutputs
      }
    | LinearMatrixLayer(_) => {
        let matrixOutputs: floatMatrix = forwardMatrixThroughMatrixLayer(layer, layerInputs)
        matrixOutputs
      }
    }
  })