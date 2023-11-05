type t
type float_array = array<float>

external toVector: float_array => t = "%identity"
external toArray: t => float_array = "%identity"

@module("mathjs") external dot: (t, t) => float = "dot"

//let empty = () => toVector([])
let length: t => int = vector => vector->toArray->Belt.Array.length
@module("mathjs") external zeros: int => t = "zeros"
let push: (t, float) => t = (vector, element) => {
  vector->toArray->Belt.Array.push(element)
  vector
}
let get: (t, int) => float = (vector, index) => vector->toArray->Belt.Array.getUnsafe(index)
let map: (t, float => float) => t = (vector, f) => vector->toArray->Belt.Array.map(f)->toVector
let reduce: (t, float, (float, float) => float) => float = (vector, initial, f) =>
  vector->toArray->Belt.Array.reduce(initial, f)