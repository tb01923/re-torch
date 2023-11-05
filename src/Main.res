open MathJs.Vector.Float
open MathJs.Matrix.Float

/***********************************
 * baseline example
 **********************************/
// data science convention is to use "x" as input variable name
let singleX = toVector([1., 2., 3., 2.5])
let x = fromArrayOfVectors([singleX, toVector([2., 5., -1., 2.]), toVector([-1.5, 2.7, 3.3, -0.8])])

let l1Weight1 = toVector([0.2, 0.8, -0.5, 1.0])
let l1Weight2 = toVector([0.5, -0.91, 0.26, -0.5])
let l1Weight3 = toVector([-0.26, -0.27, 0.17, 0.87])
let l1Weights = fromArrayOfVectors([l1Weight1, l1Weight2, l1Weight3])

let l1Bias1 = 2.
let l1Bias2 = 3.
let l1Bias3 = 0.5
let l1Biases = toMatrix([[l1Bias1, l1Bias2, l1Bias3]])

let l2Weights = fromArrayOfVectors([
  toVector([0.1, -0.14, 0.5]),
  toVector([-0.5, 0.12, -0.33]),
  toVector([-0.44, 0.73, -0.13]),
])

let l2Bias1 = -1.
let l2Bias2 = 2.
let l2Bias3 = -0.5
let l2Biases = toMatrix([[l2Bias1, l2Bias2, l2Bias3]])
let runDotProductMath = () => {
  Js.Console.log("------------------")
  Js.Console.log("Dot Product Math \n")

  let output = [
    dot(singleX, l1Weight1) +. l1Bias1,
    dot(singleX, l1Weight2) +. l1Bias2,
    dot(singleX, l1Weight3) +. l1Bias3,
  ]
  Js.Console.log(output)

  Js.Console.log("------------------")
  Js.Console.log("Matrix Math \n")
  let out = singleX->fromVector->transpose->multiply(l1Weights, _)->add(transpose(l1Biases))
  Js.Console.log(out)
}

let runSingleLinearMatrixWithSingleVectorInput = () => {
  Js.Console.log("------------------")
  Js.Console.log("Linear Matrix with single Vector Input \n")

  open Layer
  open FeedForward

  let matrixLayerRecord = {
    numInputs: 4,
    numNeurons: 3,
    weights: l1Weights,
    biases: l1Biases,
  }
  let matrixLayer = Dense({implementation: DenseMatrixLayer(matrixLayerRecord)})

  forwardSingle([matrixLayer], singleX)->ignore
  Js.Console.log(matrixLayerRecord.output)
}

let runSingleLinearMatrixBatchInput = () => {
  open Layer
  open FeedForward

  Js.Console.log("------------------")
  Js.Console.log("Linear Matrix with batch Input \n")

  let matrixLayerRecord2 = {
    numInputs: 4,
    numNeurons: 3,
    weights: transpose(l1Weights),
    biases: l1Biases,
  }
  let matrixLayer2 = Dense({implementation: DenseMatrixLayer(matrixLayerRecord2)})

  let outputs = forwardBatch([matrixLayer2], x)
  Js.Console.log(outputs)
}

let runSynapseApproachSingleVectorInput = () => {
  Js.Console.log("------------------")
  Js.Console.log("Linear synapse approach, single vector input\n")

  open Neuron
  open Layer
  open FeedForward

  // for each input we need an input neuron
  let inputLength = MathJs.Vector.Float.length(singleX)
  let inputNeurons: array<neuron> = Belt.Array.makeBy(inputLength, _ => makeInputNeuron())

  // we need some elements in the next layer to combine elements
  let endNeuron = makeOutputNeuron(dot, l1Bias1)
  let endNeuron2 = makeOutputNeuron(dot, l1Bias2)
  let endNeuron3 = makeOutputNeuron(dot, l1Bias3)

  let inputLayer = makeDenseGraphInputLayer(Some(l1Weights), inputNeurons)
  let outputLayer = makeDenseGraphLayer([endNeuron, endNeuron2, endNeuron3])
  let layers = connectLayers([inputLayer, outputLayer])

  forwardSingle(layers, singleX)->ignore
  outputLayer->getDenseLayer->getNeurons->Belt.Array.map(getNeuronOutput)->Js.Console.log
}

let runSynapseApproachBatchInput = () => {
  Js.Console.log("------------------")
  Js.Console.log("Linear synapse approach, batch input\n")

  open Neuron
  open Layer
  open FeedForward

  let inputLayer = makeDenseGraphInputLayer(
    Some(l1Weights),
    [makeInputNeuron(), makeInputNeuron(), makeInputNeuron(), makeInputNeuron()],
  )
  let layer1 = makeDenseGraphLayer([
    makeOutputNeuron(dot, l1Bias1),
    makeOutputNeuron(dot, l1Bias2),
    makeOutputNeuron(dot, l1Bias3),
  ])
  let layers = connectLayers([inputLayer, layer1])

  let outputs = forwardBatch(layers, x)
  Js.Console.log(outputs)
}

let runTwoLayerSynapseApproachBatchInput = () => {
  Js.Console.log("------------------")
  Js.Console.log("Two Layer Linear synapse approach, batch input\n")

  open Neuron
  open Layer
  open FeedForward

  let inputLayer = makeDenseGraphInputLayer(
    Some(l1Weights),
    [makeInputNeuron(), makeInputNeuron(), makeInputNeuron(), makeInputNeuron()],
  )
  let layer1 = makeDenseGraphLayer(
    ~weights=l2Weights,
    [
      makeOutputNeuron(dot, l1Bias1),
      makeOutputNeuron(dot, l1Bias2),
      makeOutputNeuron(dot, l1Bias3),
    ],
  )

  let layer2 = makeDenseGraphLayer([
    makeOutputNeuron(dot, l2Bias1),
    makeOutputNeuron(dot, l2Bias2),
    makeOutputNeuron(dot, l2Bias3),
  ])

  let network = connectLayers([inputLayer, layer1, layer2])

  let outputs = forwardBatch(network, x)
  Js.Console.log(outputs)
}

let runTwoLinearMatrixBatchInput = () => {
  open Layer
  open FeedForward

  Js.Console.log("------------------")
  Js.Console.log("Two Layer Linear Matrix with batch Input \n")

  let matrixLayer1 = Dense({
    implementation: DenseMatrixLayer({
      numInputs: 4,
      numNeurons: 3,
      weights: transpose(l1Weights),
      biases: l1Biases,
    }),
  })

  let matrixLayer2 = Dense({
    implementation: DenseMatrixLayer({
      numInputs: 3,
      numNeurons: 3,
      weights: transpose(l2Weights),
      biases: l2Biases,
    }),
  })

  let network = [matrixLayer1, matrixLayer2]

  let outputs = forwardBatch(network, x)
  Js.Console.log(outputs)
}

let runTwoLinearMatrixBatchInputGeneratedLayer = () => {
  open Layer
  open FeedForward

  Js.Console.log("------------------")
  Js.Console.log("Linear Network from constructor\n")

  let network = [
    makeDenseMatrixLayer(4, 3),
    makeReLU(),
    makeDenseMatrixLayer(3, 10),
    makeReLU(),
    makeDenseMatrixLayer(10, 60),
    makeReLU(),
    makeDenseMatrixLayer(60, 20),
    makeReLU(),
    makeDenseMatrixLayer(20, 3),
    makeSoftmax(),
  ]

  let outputs = forwardBatch(network, x)
  Js.Console.log(outputs)
}

runDotProductMath()
runSingleLinearMatrixWithSingleVectorInput()
runSingleLinearMatrixBatchInput()
runSynapseApproachSingleVectorInput()
runSynapseApproachBatchInput()
runTwoLinearMatrixBatchInput()
runTwoLayerSynapseApproachBatchInput()
runTwoLinearMatrixBatchInputGeneratedLayer()