open Matrix

/***********************************
 * baseline example
 **********************************/
let inputX = [1., 2., 3.]
let weightX = [0.2, 0.8, 0.5]
let biasX = 2.
let output = arrayDot(inputX, weightX) +. biasX
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
let endNeuron = makeOutputNeuron(arrayDot, biasX)
let endNeuron2 = makeOutputNeuron(arrayDot, biasX)
let endNeuron3 = makeOutputNeuron(arrayDot, biasX)

// l1 is the input layer
let l1 = makeLinearLayer(inputLayer, ~weights=weightX)
// l2 is the output layer
let l2 = makeLinearLayer([endNeuron])
// connect the layers
let layers = [l1, l2]
connectLayers(layers)

forward(layers, inputX)->ignore

endNeuron->getNeuronValue->Js.Console.log