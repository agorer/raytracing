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

let random () =
  let open Random in
  random_double(), random_double, random_double()

let random_between interval =
  let open Random in
  (random_between interval), (random_between interval), (random_between interval)

let rec random_unit_vector () =
  let p = random_between (-1.0, 1.0) in
  let lensq = length_squared p in
  if lensq <= 1.0 && lensq > 1e-160 then p / lensq
  else random_unit_vector()

let random_on_hemisphere normal =
  let on_unit_sphere = random_unit_vector() in
  if (dot on_unit_sphere normal) > 0.0 then on_unit_sphere
  else (neg on_unit_sphere)

let near_zero (x, y, z) =
  let threshold = 1e-8 in
  (Float.abs x) < threshold && (Float.abs y) < threshold && (Float.abs z) < threshold

let reflect v n =
  let dot_v_n = 2.0 *. (dot v n) in
  v - (n * dot_v_n)

let refract uv n etai_over_etat =
  let cos_theta = Float.min (dot (neg uv) n) 1.0 in
  let r_out_perp = (uv + (n * cos_theta)) * etai_over_etat in
  let r_out_parallel = n * (-. sqrt (Float.abs 1.0 -. (length_squared r_out_perp))) in
  r_out_perp + r_out_parallel
