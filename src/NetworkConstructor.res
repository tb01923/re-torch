open Model
open Connect

// factory functions
let rec fullyNetworkLayers = (connections, layers: list<layer>) =>
  switch layers {
  // nothing to connect if there is < 1 layer
  | list{} => connections
  | list{_} => connections
  | list{layer1, layer2, ...restLayers} => {
      // connect to layers, add to connections array (with side effect), ignore the ret-val the array size
      connectTwoLayers(layer1, layer2)->Js.Array2.push(connections, _)->ignore
      // recurse with accumulated connections, and list of layers without the first
      fullyNetworkLayers(connections, list{layer2, ...restLayers})
    }
  }

let makeFullyConnectedNetwork = layers => {
  FullyConnectedNetwork(Uuid.V4.make(), fullyNetworkLayers([], layers))
}