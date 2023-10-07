open Model

let formatNeuron = n => {
  `Neuron{label="${n.label}"\
  bias="${Js.Float.toString(n.bias)}"\
  value="${Js.Float.toString(n.value)}"}`
}

let formatConnection = connection => {
  let (left, right) = connection
  `${formatNeuron(left)} -> ${formatNeuron(right)}`
}

let printConnections = (connectionList: list<connection>) =>
  connectionList->Belt.List.map(formatConnection)->Belt.List.forEach(Js.Console.log)

let printConnectionBetweenLayers = connectionsArr =>
  Js.Array2.forEach(connectionsArr, connections => {
    Js.Console.log(`----------------------------------------------------------`)
    printConnections(connections)
  })

let printNetwork = model =>
  switch model {
  | FullyConnectedNetwork(uuid, connectionsArr) => {
      Js.Console.log(`FullyConnectedNetwork(${uuid})`)
      printConnectionBetweenLayers(connectionsArr)
    }
  }