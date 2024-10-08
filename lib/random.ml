let random_between (min, max) =
  min +. (max -. min) *. (Stdlib.Random.float 1.)

let random_double () =
  random_between (0.0, 1.0)
