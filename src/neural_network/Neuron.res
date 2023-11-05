open Shared

type neuronRecord = {
  fn: (MathJs.Vector.Float.t, MathJs.Vector.Float.t) => float,
  bias?: float,
  mutable output?: float,
}

type inputNeuronRecord = {
  fn: float => float,
  mutable output?: float,
}

type neuron =
  | InputNeuron(uuid, inputNeuronRecord)
  | OutputNeuron(uuid, neuronRecord)

let makeInputNeuron = () => InputNeuron(makeUUID(), {fn: id})
let makeOutputNeuron = (fn, bias) => OutputNeuron(makeUUID(), {fn, bias})

// given any neuron extract the uuid
let getNeuronId = n =>
  switch n {
  | InputNeuron(uuid, _) => uuid
  | OutputNeuron(uuid, _) => uuid
  }

exception NeuronOutputNotSet(neuron)
let getNeuronOutput: neuron => float = n => {
  let someOutput = switch n {
  | InputNeuron(_, {?output}) => output
  | OutputNeuron(_, {?output}) => output
  }
  switch someOutput {
  | Some(output) => output
  | None => raise(NeuronOutputNotSet(n))
  }
}

exception NotInputNeuron(neuron)
let getInputNeuronRecord = n =>
  switch n {
  | InputNeuron(_, r) => r
  | OutputNeuron(_, _) => raise(NotInputNeuron(n))
  }

exception NotChainedNeuron(neuron)
let getChainedNeuronRecord = n =>
  switch n {
  | InputNeuron(_, _) => raise(NotChainedNeuron(n))
  | OutputNeuron(_, r) => r
  }