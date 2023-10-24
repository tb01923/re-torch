type t
type float_matrix = array<array<float>>

external toMatrix: float_matrix => t = "%identity"
external fromArrayOfVectors: array<MathJs_Vector_Float.t> => t = "%identity"
external toArrayOfVectors: t => array<MathJs_Vector_Float.t> = "%identity"

//@module("mathjs") external emptyMatrix: unit => t = "matrix"
//@module("mathjs") external toMatrix: float_matrix => t = "matrix"

@module("mathjs")
external multiply: (t, t) => t = "multiply"

@module("mathjs") external transpose: t => t = "transpose"
@module("mathjs") external add: (t, t) => t = "add"

let fromVector: MathJs_Vector_Float.t => t = vector => fromArrayOfVectors([vector])