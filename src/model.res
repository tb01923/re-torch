type uuid = string
type neuron = {
  label: uuid,
  mutable bias: float,
  mutable value: float,
}
type connection = (neuron, neuron)
type model = FullyConnectedNetwork(uuid, array<list<connection>>)
type layer = Linear(uuid, list<neuron>)

let makeNeuron = (bias, value) => {label: Uuid.V4.make(), bias, value}
let makeLabeledNeuron = (label, bias, value) => {label, bias, value}
let makeLayer = neurons => Linear(Uuid.V4.make(), neurons)
let makeLabeledLayer = (label, neurons) => Linear(label, neurons)
let makeConnection = (n1, n2): connection => (n1, n2)