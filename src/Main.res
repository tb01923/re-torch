open Matrix

/***********************************
 * baseline example
 **********************************/
let inputX = [1., 2., 3., 2.5]
let weight1 = [0.2, 0.8, -0.5, 1.0]
let weight2 = [0.5, -0.91, 0.26, -0.5]
let weight3 = [-0.26, -0.27, 0.17, 0.87]
let weightMatrix = [weight1, weight2, weight3]

let bias1 = 2.
let bias2 = 3.
let bias3 = 0.5

let output = [
  arrayDot(inputX, weight1) +. bias1,
  arrayDot(inputX, weight2) +. bias2,
  arrayDot(inputX, weight3) +. bias3,
]
Js.Console.log(output)

/***********************************
 * modeled solution 1 types
 **********************************/
open Neuron
open Layer
open FeedForward

// for each input we need an input neuron
let inputLayer = Js.Array2.map(inputX, _ => makeInputNeuron())
// we need some elements in the next layer to combine elements
let endNeuron = makeOutputNeuron(arrayDot, bias1)
let endNeuron2 = makeOutputNeuron(arrayDot, bias2)
let endNeuron3 = makeOutputNeuron(arrayDot, bias3)

// l1 is the input layer
let l1 = makeLinearInputLayer(inputLayer, ~weights=weightMatrix)
// l2 is the output layer
let l2 = makeLinearLayer([endNeuron, endNeuron2, endNeuron3])
// connect the layers
let layers = [l1, l2]
connectLayers(layers)

forward(layers, inputX)->Shared.last->getNeurons->Belt.Array.map(getNeuronValue)->Js.Console.log