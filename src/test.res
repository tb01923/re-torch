let test = (~i: option<int>=?) =>
  switch i {
  | Some(i) => i * i
  | None => 0
  }

test(~i=10)->ignore