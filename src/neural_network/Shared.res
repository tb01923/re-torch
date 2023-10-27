// identity function (take an x and return an x)
let id = x => x

type uuid = string
let makeUUID = Uuid.V4.make

// type aliases for readability
type input = float
type weight = float
type bias = float

type floatMatrix = MathJs.Matrix.Float.t
type floatVector = MathJs.Vector.Float.t

let default = (someFloat, defaultFloat) =>
  switch someFloat {
  | Some(f) => f
  | None => defaultFloat
  }