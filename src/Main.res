open MathJs.Vector.Float
open MathJs.Matrix.Float

/***********************************
 * baseline example
 **********************************/
let inputs: MathJs.Vector.Float.t = toVector([1., 2., 3., 2.5])
let weight1 = toVector([0.2, 0.8, -0.5, 1.0])
let weight2 = toVector([0.5, -0.91, 0.26, -0.5])
let weight3 = toVector([-0.26, -0.27, 0.17, 0.87])
let weights = fromArrayOfVectors([weight1, weight2, weight3])

let bias1 = 2.
let bias2 = 3.
let bias3 = 0.5

let biases = toMatrix([[bias1, bias2, bias3]])

let output = [
  dot(inputs, weight1) +. bias1,
  dot(inputs, weight2) +. bias2,
  dot(inputs, weight3) +. bias3,
]
Js.Console.log(output)

Js.Console.log("------------------")
let out = inputs->fromVector->transpose->multiply(weights, _)->add(transpose(biases))
Js.Console.log(out)

Js.Console.log("------------------")

/***********************************
 * modeled solution 1 types
 **********************************/
open Neuron
open Layer
open FeedForward

// for each input we need an input neuron
let inputLength = MathJs.Vector.Float.length(inputs)
let inputNeurons: array<neuron> = Belt.Array.makeBy(inputLength, _ => makeInputNeuron())

// we need some elements in the next layer to combine elements
let endNeuron = makeOutputNeuron(dot, bias1)
let endNeuron2 = makeOutputNeuron(dot, bias2)
let endNeuron3 = makeOutputNeuron(dot, bias3)

let inputLayer = makeLinearInputLayer(inputNeurons, ~weights)
let outputLayer = makeLinearLayer([endNeuron, endNeuron2, endNeuron3])
let layers = connectLayers([inputLayer, outputLayer])

forward(layers, inputs)->ignore
outputLayer->getNeurons->Belt.Array.map(getNeuronValue)->Js.Console.log