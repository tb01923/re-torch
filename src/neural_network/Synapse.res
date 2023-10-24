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

type neurons = array<neuron>
let makeSynapses = (neurons1: neurons, weights: floatMatrix, neurons2: neurons): array<synapse> => {
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

  open MathJs.Vector.Float
  open MathJs.Matrix.Float

  let pair = ((n2, inputWeights: floatVector)) => {
    let _makeSynapse = ((n1, w)) => makeSynapse(n1, w, n2)
    let zipped = Belt.Array.zip(neurons1, toArray(inputWeights))
    let mapped = Belt.Array.map(zipped, _makeSynapse)
    mapped
  }

  let zipped = Belt.Array.zip(neurons2, toArrayOfVectors(weights))
  let mapped = Belt.Array.flatMap(zipped, pair)
  mapped
}