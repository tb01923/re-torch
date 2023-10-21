open Shared
open Neuron

// synapse: (aka connection) between to neurons with a weight
type synapse = (neuron, weight, neuron)
let makeSynapse = (n1, w, n2) => (n1, w, n2)

let getOutputNeuron = synapse =>
  switch synapse {
  | (_, _, output) => output
  }
//given a synapse, extract the uuid of the output neuron
let getSynapseOutputNeuronId = synapse => synapse->getOutputNeuron->getNeuronId

let makeSynapses = (
  neurons1: array<neuron>,
  weights: array<array<weight>>,
  neurons2: array<neuron>,
): array<synapse> => {
  /*
    dimensions(neurons1) = 1
    |neurons1| = n
    dimensions(neurons2) = 1
    |neurons2| = m
    dimensions(weights) = m x n

    1.  zip(neurons2, w)                => [neuron, [weights]]
        // |[neuron, [weights]]| = m
        // |weights| = n
    2.  flatMap (step1)                 => [synapse]
    2a. zip(step 1.weights, neurons1)   => [neuron, [weights]]
        // |[neuron, [weights]]| = n
        // |weights| = m
    2b. map(step 2a)                    => [[synapse]]
 */

  let pair = ((n2, inputWeights)) =>
    Belt.Array.zip(neurons1, inputWeights)->Belt.Array.map(((n1, w)) => makeSynapse(n1, w, n2))

  Belt.Array.zip(neurons2, weights)->Belt.Array.flatMap(pair)
}