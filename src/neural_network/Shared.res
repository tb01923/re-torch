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

// function from (input,  weight) to a float
type input_and_weight = (array<input>, array<weight>)
type input_and_weight_to_float = (array<input>, array<weight>) => float

let default = (someFloat, defaultFloat) =>
  switch someFloat {
  | Some(f) => f
  | None => defaultFloat
  }