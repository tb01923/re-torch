open Model
open NetworkConstructor
open Pretty

// main code
let layer1 = makeLabeledLayer(
  "layer-1",
  list{makeLabeledNeuron("layer1-1", 1., 7.), makeLabeledNeuron("layer1-2", 2., 8.)},
)

let layer2 = makeLabeledLayer(
  "layer-2",
  list{makeLabeledNeuron("layer2-1", 3., 9.), makeLabeledNeuron("layer2-2", 4., 10.)},
)

let layer3 = makeLabeledLayer(
  "layer-3",
  list{makeLabeledNeuron("layer3-1", 3., 9.), makeLabeledNeuron("layer3-2", 4., 10.)},
)

let net = makeFullyConnectedNetwork(list{layer1, layer2, layer3})
printNetwork(net)