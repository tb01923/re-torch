open Shared
open Neuron

// synapse: (aka connection) between to neurons with a weight
type synapse = (neuron, weight, neuron)
let makeSynapse = (n1, w, n2) => (n1, w, n2)

//given a synapse, extract the uuid of the output neuron
let getSynapseOutputNeuronId = synapse =>
  switch synapse {
  | (_, _, output) => getNeuronId(output)
  }

let makeSynapses = (neurons1, weights, neurons2): array<synapse> => {
  let pairWithNeurons2 = ((n1, w)) => Belt.Array.map(neurons2, n2 => makeSynapse(n1, w, n2))
  Belt.Array.zip(neurons1, weights)->Belt.Array.flatMap(pairWithNeurons2)
}