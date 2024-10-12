type t = Vec3d.t

let random_beetwen interval =
  let open Random in
  random_between interval, random_between interval, random_between interval
    
let random () =
  random_beetwen (0.0, 1.0)

let linear_to_gamma linear =
  if linear > 0.0 then Float.sqrt linear
  else 0.0
  
let write_color out_channel (r, g, b) =
  let open Interval in
  let factor = 256. in
  let intensity = 0.0, 0.999 in
  let r = linear_to_gamma r in
  let g = linear_to_gamma g in
  let b = linear_to_gamma b in
  let ir = (int_of_float (factor *. (clamp r intensity))) in
  let ig = (int_of_float (factor *. (clamp g intensity))) in
  let ib = (int_of_float (factor *. (clamp b intensity))) in
  Printf.fprintf out_channel "%d %d %d\n" ir ig ib
