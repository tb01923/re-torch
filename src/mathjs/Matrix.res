type t
type float_array = array<float>
type float_jagged_array = array<float_array>
type int_array = array<int>
type int_jagged_array = array<int_array>

@module("mathjs") external emptyMatrix: unit => t = "matrix"
@module("mathjs") external floatMatrix: float_jagged_array => t = "matrix"
@module("mathjs") external intMatrix: int_jagged_array => t = "matrix"
@module("mathjs") external arrayDot: (float_array, float_array) => float = "dot"