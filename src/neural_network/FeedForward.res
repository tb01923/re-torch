open Shared
open Neuron
open Layer
open Synapse

// todo: handle |inputs| != |layer.neurons|
let forwardFromInput = (layer, inputs) => {
  let get = MathJs.Vector.Float.get
  let neurons = getNeurons(layer)
  Belt.Array.forEachWithIndex(neurons, (i, neuron) => {
    let inputRecord = getInputNeuronRecord(neuron)
    inputRecord.value = Some(inputRecord.fn(get(inputs, i)))
  })
  layer
}

let getValuesAndWeightsFromSynapses: array<synapse> => (
  MathJs.Vector.Float.t,
  MathJs.Vector.Float.t,
) = synapses => {
  let emptyVector = MathJs.Vector.Float.empty
  // for each synapse reduce onto a pair of empty arrays by pushing the input neuron value and the weight
  Belt.Array.reduce(synapses, (emptyVector(), emptyVector()), (agg, synapse) => {
    let (n1, weight, _) = synapse
    let (values: MathJs.Vector.Float.t, weights) = agg
    let value = getNeuronValue(n1)
    MathJs.Vector.Float.push(values, value)->ignore
    MathJs.Vector.Float.push(weights, weight)->ignore
    (values, weights)
  })
}

let feedNeuron = synapses => {
  // extract output neuron from the first synapse (they should all have the same neuron)
  let (_, _, n) = synapses[0]
  // get the values from all the input neurons, and the weights of the synapses
  let (values, weights) = getValuesAndWeightsFromSynapses(synapses)
  // with the output neuron's record
  let neuronRecord = getChainedNeuronRecord(n)
  // get the bias (default 0)
  let bias = default(neuronRecord.bias, 0.)

  // apply the inputs and weights to it's calculation function and add the bias
  neuronRecord.value = Some(neuronRecord.fn(weights, values) +. bias)
}

let petitionSynapsesByOutputNeuron = (outputNeuronToMatch, synapses) =>
  Belt.Array.partition(synapses, anotherSynapse => {
    let outputNeuron = getOutputNeuron(anotherSynapse)
    getNeuronId(outputNeuron) == getNeuronId(outputNeuronToMatch)
  })

let forwardFromPriorLayer = layer => {
  let rec _feedThrough = synapses => {
    switch Belt.Array.get(synapses, 0) {
    | None => ignore()
    | Some(synapse) => {
        // group all synapses that have the current output, and everything else
        let (matchedSynapses, unmatchedSynapses) = petitionSynapsesByOutputNeuron(
          getOutputNeuron(synapse),
          synapses,
        )
        // all the synapses in the matched set feed their value into that output neuron
        feedNeuron(matchedSynapses)
        // the remainder of the synapses still need to have have their inputs forward their values
        _feedThrough(unmatchedSynapses)
      }
    }->ignore
  }

  getSynapses(layer)->_feedThrough
  layer
}

let forward: (array<layer>, floatVector) => array<layer> = (layers, inputs) => {
  Belt.Array.map(layers, layer =>
    switch layer {
    | LinearInputLayer(_) => forwardFromInput(layer, inputs)
    | LinearLayer(_) => forwardFromPriorLayer(layer)
    }
  )
}