type t = float * float * float

let zero = 0, 0, 0

let create x y z = x, y, z

let x (x, _, _) = x

let y (_, y, _) = y

let z (_, _, z) = z

let neg (x, y, z) = -.x, -.y, -.z

let ( - ) (x1, y1, z1) (x2, y2, z2) = x1 -. x2, y1 -. y2, z1 -. z2

let ( + ) (x1, y1, z1) (x2, y2, z2) = x1 +. x2, y1 +. y2, z1 +. z2

let ( * ) (x, y, z) t = t *. x, t *. y, t *. z

let ( ** ) (x1, y1, z1) (x2, y2, z2) = x1 *. x2, y1 *. y2, z1 *. z2 

let ( / ) (x, y, z) t = x /. t, y /. t, z /. t

let length_squared (x, y, z) = (x *. x) +. (y *. y) +. (z *. z)

let length v = sqrt (length_squared v)

let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let cross (x1, y1, z1) (x2, y2, z2) =
  (y1 *. z2) -. (z1 *. y2),
  (z1 *. x2) -. (x1 *. z2),
  (x1 *. y2) -. (y1 *. x2)

let unit_vector v = v / (length v)

let string_of_vec3d (x, y, z) =
  (string_of_float x) ^ " " ^ (string_of_float y) ^ " " ^ (string_of_float z)
