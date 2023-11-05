open Belt.Int
/*
def create_data(samples, classes):
    X = np.zeros((samples*classes, 2))
    y = np.zeros(samples*classes, dtype='uint8')
    for class_number in range(classes):
        ix = range(samples*class_number, samples*(class_number+1))
        r = np.linspace(0.0, 1, samples)
        t = np.linspace(class_number*4, (class_number+1)*4, samples) + np.random.randn(samples)*0.2
        X[ix] = np.c_[r*np.sin(t*2.5), r*np.cos(t*2.5)]
        y[ix] = class_number
    return X, y
*/
let createSpiralData = (samples, classes) => {
  let classesLengthArray = MathJs.General.range(1, classes, true)
  let sampleLengthArray = MathJs.General.range(1, samples, true)

  classesLengthArray->Belt.Array.reduce(([], []), (aggregate, classNumber) => {
    let (xs, ys) = aggregate

    //r = np.linspace(0.0, 1, samples)
    let r = MathJs.General.linearSpace(0.0, 1.0, toFloat(samples))

    let t = // np.linspace(class_number*4, (class_number+1)*4, samples)
    MathJs.General.linearSpace(
      toFloat(classNumber * 4),
      toFloat((classNumber + 1) * 4),
      toFloat(samples),
    )
    //+ np.random.randn(samples)*0.2
    ->Belt.Array.map(x => x +. MathJs.General.gaussianRandom(0.0, 1.0) *. 0.2)

    let sinT =
      t
      // np.sin(t*2.5)...
      ->Belt.Array.map(t => MathJs.General.sin(t *. 2.5))
      // ...r*np.sin(t*2.5)
      ->Belt.Array.zip(r)
      ->Belt.Array.map(((t2_5, r)) => t2_5 *. r)

    let cosT =
      t
      //np.cos(t*2.5)...
      ->Belt.Array.map(t => MathJs.General.cos(t *. 2.5))
      // ... r*np.cos(t*2.5)
      ->Belt.Array.zip(r)
      ->Belt.Array.map(((a, b)) => a *. b)

    let xs2 = Belt.Array.concat(xs, Belt.Array.zip(sinT, cosT))

    let ys2 = sampleLengthArray->Belt.Array.map(_ => classNumber)->Belt.Array.concat(ys, _)

    (xs2, ys2)
  })
}

createSpiralData(10, 3)
->(((x, y)) => Belt.Array.zip(x, y))
->Belt.Array.map((((x, y), yhat)) =>
  Js.Console.log(
    `${Js.Float.toPrecision(x)}, ${Js.Float.toPrecision(y)}, ${Js.Int.toString(yhat)},`,
  )
)
->ignore