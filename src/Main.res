open MathJs.Vector.Float
open MathJs.Matrix.Float

/***********************************
 * baseline example
 **********************************/
let inputVector = toVector([1., 2., 3., 2.5])
let inputBatch = fromArrayOfVectors([
  inputVector,
  toVector([2., 5., -1., 2.]),
  toVector([-1.5, 2.7, 3.3, -0.8]),
])

let weight1 = toVector([0.2, 0.8, -0.5, 1.0])
let weight2 = toVector([0.5, -0.91, 0.26, -0.5])
let weight3 = toVector([-0.26, -0.27, 0.17, 0.87])
let weights = fromArrayOfVectors([weight1, weight2, weight3])

let bias1 = 2.
let bias2 = 3.
let bias3 = 0.5

let biases = toMatrix([[bias1, bias2, bias3]])

Js.Console.log("------------------")
Js.Console.log("Dot Product Math \n")

let output = [
  dot(inputVector, weight1) +. bias1,
  dot(inputVector, weight2) +. bias2,
  dot(inputVector, weight3) +. bias3,
]
Js.Console.log(output)

Js.Console.log("------------------")
Js.Console.log("Matrix Math \n")
let out = inputVector->fromVector->transpose->multiply(weights, _)->add(transpose(biases))
Js.Console.log(out)

Js.Console.log("------------------")
Js.Console.log("Linear Matrix with single Vector Input \n")

open Layer
open FeedForward

let matrixLayerRecord = {
  inputNeuronCount: 4,
  outputNeuronCount: 3,
  weights,
  biases,
}
let matrixLayer = LinearMatrixLayer(matrixLayerRecord)

forwardVector([matrixLayer], inputVector)->ignore
Js.Console.log(matrixLayerRecord.values)
Js.Console.log("------------------")
Js.Console.log("Linear Matrix with batch Input \n")

let matrixLayerRecord2 = {
  inputNeuronCount: 4,
  outputNeuronCount: 3,
  weights,
  biases,
}
let matrixLayer2 = LinearMatrixLayer(matrixLayerRecord2)

let outputs = forwardMatrix([matrixLayer2], inputBatch)
Js.Console.log(outputs)
Js.Console.log("------------------")
Js.Console.log("Linear synapse approach\n")

/***********************************
 * modeled solution 1 types
 **********************************/
let run = () => {
  open Neuron

  // for each input we need an input neuron
  let inputLength = MathJs.Vector.Float.length(inputVector)
  let inputNeurons: array<neuron> = Belt.Array.makeBy(inputLength, _ => makeInputNeuron())

  // we need some elements in the next layer to combine elements
  let endNeuron = makeOutputNeuron(dot, bias1)
  let endNeuron2 = makeOutputNeuron(dot, bias2)
  let endNeuron3 = makeOutputNeuron(dot, bias3)

  let inputLayer = makeLinearInputLayer(inputNeurons, ~weights)
  let outputLayer = makeLinearLayer([endNeuron, endNeuron2, endNeuron3])
  let layers = connectLayers([inputLayer, outputLayer])

  forwardVector(layers, inputVector)->ignore
  outputLayer->getNeurons->Belt.Array.map(getNeuronValue)->Js.Console.log
}
run()

Js.Console.log("------------------")
Js.Console.log("Linear synapse approach, batch input\n")
let run2 = () => {
  open Neuron

  // for each input we need an input neuron
  let inputLength = MathJs.Vector.Float.length(inputVector)
  let inputNeurons: array<neuron> = Belt.Array.makeBy(inputLength, _ => makeInputNeuron())

  // we need some elements in the next layer to combine elements
  let endNeuron = makeOutputNeuron(dot, bias1)
  let endNeuron2 = makeOutputNeuron(dot, bias2)
  let endNeuron3 = makeOutputNeuron(dot, bias3)

  let inputLayer = makeLinearInputLayer(inputNeurons, ~weights)
  let outputLayer = makeLinearLayer([endNeuron, endNeuron2, endNeuron3])
  let layers = connectLayers([inputLayer, outputLayer])

  let outputs = forwardMatrix(layers, inputBatch)
  Js.Console.log(outputs)
  Js.Console.log("------------------")
}
run2()