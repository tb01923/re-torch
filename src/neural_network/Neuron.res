open Shared

type neuronRecord = {
  fn: (MathJs.Vector.Float.t, MathJs.Vector.Float.t) => float,
  bias?: float,
  mutable value?: float,
}

type inputNeuronRecord = {
  fn: float => float,
  mutable value?: float,
}

type neuron =
  | InputNeuron(uuid, inputNeuronRecord)
  | MiddleNeuron(uuid, neuronRecord)
  | OutputNeuron(uuid, neuronRecord)

let makeInputNeuron = () => InputNeuron(makeUUID(), {fn: id})
let makeOutputNeuron = (fn, bias) => OutputNeuron(makeUUID(), {fn, bias})

// given any neuron extract the uuid
let getNeuronId = n =>
  switch n {
  | InputNeuron(uuid, _) => uuid
  | MiddleNeuron(uuid, _) => uuid
  | OutputNeuron(uuid, _) => uuid
  }

exception NeuronValueNotSet(neuron)
let getNeuronValue: neuron => float = n => {
  let someValue = switch n {
  | InputNeuron(_, {?value}) => value
  | MiddleNeuron(_, {?value}) => value
  | OutputNeuron(_, {?value}) => value
  }
  switch someValue {
  | Some(value) => value
  | None => raise(NeuronValueNotSet(n))
  }
}

exception NotInputNeuron(neuron)
let getInputNeuronRecord = n =>
  switch n {
  | InputNeuron(_, r) => r
  | MiddleNeuron(_, _) => raise(NotInputNeuron(n))
  | OutputNeuron(_, _) => raise(NotInputNeuron(n))
  }

exception NotChainedNeuron(neuron)
let getChainedNeuronRecord = n =>
  switch n {
  | InputNeuron(_, _) => raise(NotChainedNeuron(n))
  | MiddleNeuron(_, r) => r
  | OutputNeuron(_, r) => r
  }