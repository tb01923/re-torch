open Shared
open Neuron
open Layer

let feedInput = (layer, inputs) => {
  let neurons = getNeurons(layer)
  Belt.Array.forEachWithIndex(neurons, (i, neuron) => {
    let inputRecord = getInputNeuronRecord(neuron)
    inputRecord.value = Some(inputRecord.fn(inputs[i]))
  })
}

let getValuesAndWeightsFromSynapses = synapses => {
  Belt.Array.reduce(synapses, ([], []), (agg, synapse) => {
    let (n1, weight, _) = synapse
    let (values: array<input>, weights) = agg
    let value = getNeuronValue(n1)
    Belt.Array.push(values, value)->ignore
    Belt.Array.push(weights, weight)->ignore
    (values, weights)
  })
}

let feedNeuron = synapses => {
  // extract output neuron from the first synapse (they should all have the same neuron)
  let (_, _, n) = synapses[0]
  let (values, weights) = getValuesAndWeightsFromSynapses(synapses)
  let neuronRecord = getChainedNeuronRecord(n)
  let bias = default(neuronRecord.bias, 0.)
  neuronRecord.value = Some(neuronRecord.fn(values, weights) +. bias)
}

let forwardThroughSynapses = layer => {
  let rec _feedThrough = synapses => {
    switch Belt.Array.get(synapses, 0) {
    | None => ignore()
    | Some((_, _, n2)) => {
        let (inputSynapse, remainder) = Belt.Array.partition(synapses, ((_, _, n1)) =>
          getNeuronId(n1) == getNeuronId(n2)
        )
        feedNeuron(inputSynapse)
        _feedThrough(remainder)
      }
    }->ignore
  }
  _feedThrough(getSynapses(layer))
}

let forward: (array<layer>, array<input>) => array<layer> = (layers, inputs) => {
  let rec _forward = (i, layers) =>
    switch (i, Belt.Array.get(layers, i)) {
    | (0, Some(layer)) => {
        feedInput(layer, inputs)
        _forward(i + 1, layers)
      }
    | (_, Some(layer)) => {
        forwardThroughSynapses(layer)
        _forward(i + 1, layers)
      }
    | (_, None) => layers
    }

  _forward(0, layers)
}