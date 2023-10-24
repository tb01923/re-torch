open Shared
open Neuron
open Synapse

type layerRecord = {
  neurons: array<neuron>,
  mutable initialWeights?: floatMatrix,
  mutable synapses?: array<synapse>,
}

type layer =
  | LinearLayer(layerRecord)
  | LinearInputLayer(layerRecord)

let makeLinearLayer = (~weights: option<floatMatrix>=?, neurons) =>
  switch weights {
  | Some(w) => LinearLayer({neurons, initialWeights: w})
  | _ => LinearLayer({neurons: neurons})
  }

let makeLinearInputLayer = (~weights: option<floatMatrix>=?, neurons) =>
  switch weights {
  | Some(w) => LinearInputLayer({neurons, initialWeights: w})
  | _ => LinearInputLayer({neurons: neurons})
  }

let getLayerRecord = layer =>
  switch layer {
  | LinearInputLayer(r) => r
  | LinearLayer(r) => r
  }

let getNeurons = layer =>
  switch layer {
  | LinearInputLayer({neurons, _}) => neurons
  | LinearLayer({neurons, _}) => neurons
  }

let getSynapses = layer =>
  switch getLayerRecord(layer).synapses {
  | Some(synapses) => synapses
  | _ => []
  }

exception IllogicalLayerConnection(layer, layer)
exception WeightlessSynapse(layer, layer)

let connectLayer1ToLayer2 = (layer1, layer2) => {
  let connectLayers = (layerRecord1, layerRecord2) => {
    let {neurons: neurons1, ?initialWeights, ?synapses} = layerRecord1
    let {neurons: neurons2, _} = layerRecord2
    switch (initialWeights, synapses) {
    | (None, None) => raise(WeightlessSynapse(layer1, layer2))
    | (Some(w), None) => {
        let synapses = makeSynapses(neurons1, w, neurons2)
        layerRecord1.synapses = Some(synapses)
        layerRecord2.synapses = Some(synapses)
      }
    | (_, Some(synapseArray)) => ignore(synapseArray)
    }
  }
  switch (layer1, layer2) {
  | (LinearInputLayer(layerRecord1), LinearLayer(layerRecord2)) =>
    connectLayers(layerRecord1, layerRecord2)
  | (LinearLayer(layerRecord1), LinearLayer(layerRecord2)) =>
    connectLayers(layerRecord1, layerRecord2)
  | (_, _) => raise(IllogicalLayerConnection(layer1, layer2))
  }
  layer1
}

let connectLayers = (layers: array<layer>) => {
  let rec _connectLayers = (layers: array<layer>) => {
    switch layers {
    | [] => ignore()
    | [_] => ignore()
    | [l1, l2] => connectLayer1ToLayer2(l1, l2)->ignore
    | _ => {
        connectLayer1ToLayer2(layers[0], layers[1])->ignore
        Js.Array2.shift(layers)->ignore
        _connectLayers(layers)
      }
    }
  }
  _connectLayers(layers)
  layers
}