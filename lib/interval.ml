type t = float * float

let empty = Float.infinity, Float.neg_infinity

let universe = Float.neg_infinity, Float.infinity

let size (min, max) = max -. min

let between n (min, max) =
  min <= n && n <= max

let surrounds (min, max) n =
  min < n && n < max

let clamp x (min, max) =
  if x < min then min
  else if x > max then max
  else x
