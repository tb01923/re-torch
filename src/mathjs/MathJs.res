module Matrix = MathJs_Matrix
module Vector = MathJs_Vector
module Statistics = MathJs_Statistics
module General = {
  let toFloat = Belt.Int.toFloat
  let fromFloat = Belt.Int.fromFloat
  type denseMatrix
  @module("mathjs") external _range: (int, int, bool) => denseMatrix = "range"
  @module("mathjs") external random: int => float = "random"
  @module("mathjs") external randomBetween: (int, int) => float = "random"
  @module("mathjs") external sin: float => float = "sin"
  @module("mathjs") external cos: float => float = "cos"
  @module("mathjs") external sqrt: float => float = "sqrt"
  @module("mathjs") external log: float => float = "log"
  @module("mathjs") external pow: (float, float) => float = "pow"
  @module("mathjs") @val external pi: float = "pi"
  @module("mathjs") @val external e: float = "e"

  @send external toArray: denseMatrix => array<int> = "toArray"
  let range = (start, end, inclusive) => _range(start, end, inclusive)->toArray

  // numpy "linspace" implementation
  let linearSpace = (startValue: float, stopValue: float, cardinality: float) => {
    let step = (stopValue -. startValue) /. (cardinality -. 1.0)
    range(0, fromFloat(cardinality), false)->Belt.Array.reduce([], (agg, i) => {
      Belt.Array.push(agg, startValue +. step *. toFloat(i))
      agg
    })
  }

  //https://stackoverflow.com/questions/25582882/javascript-math-random-normal-distribution-gaussian-bell-curve
  let gaussianRandom = (mean, stdev) => {
    let u = 1.0 -. randomBetween(0, 1)
    let v = randomBetween(0, 1)
    let z = sqrt(-2.0 *. log(u)) *. cos(2.0 *. pi *. v)
    z *. stdev +. mean
  }
}