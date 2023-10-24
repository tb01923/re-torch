// identity function (take an x and return an x)
let id = x => x
// function for a constant (given an x, return a null function, that returns x)
let constant = (x, ()) => x

type uuid = string
let makeUUID = Uuid.V4.make

// type aliases for readability
type input = float
type weight = float
type bias = float

type floatMatrix = MathJs.Matrix.Float.t
type floatVector = MathJs.Vector.Float.t

// function from (input,  weight) to a float
//type weight_and_input = (array<weight>, array<input>)
//type weight_and_input_to_float = (array<weight>, array<input>) => float

let default = (someFloat, defaultFloat) =>
  switch someFloat {
  | Some(f) => f
  | None => defaultFloat
  }

let last = arr => {
  let l = Belt.Array.length(arr)
  Belt.Array.getUnsafe(arr, l - 1)
}