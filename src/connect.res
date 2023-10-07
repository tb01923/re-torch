open Model

// connect neurons between layers
exception NoConnectable(layer, layer)

let rec connectNeuron = (connections: list<connection>, singleNeuron, neuronList) => {
  switch neuronList {
  | list{} => connections
  | list{singleNeuron2, ...restNeurons2} => {
      let newConnection = makeConnection(singleNeuron, singleNeuron2)
      let newConnections = Belt.List.add(connections, newConnection)
      connectNeuron(newConnections, singleNeuron, restNeurons2)
    }
  }
}

let rec connectNeurons = (connections, neurons1, neurons2) => {
  switch neurons1 {
  | list{} => connections
  | list{neuron1, ...restNeurons1} => {
      let newConnections = connectNeuron(connections, neuron1, neurons2)
      connectNeurons(newConnections, restNeurons1, neurons2)
    }
  }
}

let connectTwoLayers = (layer1: layer, layer2: layer) => {
  switch (layer1, layer2) {
  | (Linear(_, neurons1), Linear(_, neurons2)) => connectNeurons(list{}, neurons1, neurons2)
  //  | _ => raise(NoConnectable(layer1, layer2))
  }
}