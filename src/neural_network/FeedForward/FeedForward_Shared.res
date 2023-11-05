open Shared
open Layer
open Synapse
open MathJs.Vector.Float

let petitionSynapsesByOutputNeuron = (outputNeuronToMatch, synapses) =>
  Belt.Array.partition(synapses, anotherSynapse => {
    let outputNeuron = getOutputNeuron(anotherSynapse)
    Neuron.getNeuronId(outputNeuron) == Neuron.getNeuronId(outputNeuronToMatch)
  })

// todo: handle |inputs| != |layer.neurons|
let feedInput = (layer, inputs) => {
  let get = MathJs.Vector.Float.get
  let toVector = MathJs.Vector.Float.toVector

  // map over each neuron in the layer
  let neurons = getNeurons(layer)
  let values = Belt.Array.mapWithIndex(neurons, (i, neuron) => {
    let neuronRecord = Neuron.getInputNeuronRecord(neuron)
    // todo: this calculation is right in this place, but seems poor design
    // get the i'th input from the input vector, and apply the neurons function
    let value = neuronRecord.fn(get(inputs, i))

    // set the value of the neuron
    neuronRecord.output = Some(value)

    // return the value
    value
  })
  // convert array of values to vector of values
  toVector(values)
}

let feedPriorLayerOutput = (layer, inputs) => {
  let neurons = getNeurons(layer)
  let synapses = getInputSynapses(layer)
  let values = Belt.Array.map(neurons, neuron => {
    let neuronRecord = Neuron.getChainedNeuronRecord(neuron)

    // get weights from synapses
    let (matchedSynapses, _) = petitionSynapsesByOutputNeuron(neuron, synapses)
    let weights = Belt.Array.map(matchedSynapses, ((_, weight, _)) => weight)->toVector

    // get the bias (default 0)
    let bias = default(neuronRecord.bias, 0.)

    // todo: this calculation is right in this place, but seems poor design
    // get the i'th input from the input vector, and apply the neurons function
    let value = neuronRecord.fn(weights, inputs) +. bias

    // set the value of the neuron
    neuronRecord.output = Some(value)

    // return the value
    value
  })
  // convert array of values to vector of values
  toVector(values)
}

let feedActivation = (layer, inputs) =>
  switch layer {
  | Neuron({fn}) => map(inputs, fn)
  | Layer({fn}) => fn(inputs)
  }