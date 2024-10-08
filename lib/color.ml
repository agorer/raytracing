type t = Vec3d.t

let write_color out_channel (r, g, b) =
  let open Interval in
  let factor = 256. in
  let intensity = 0.0, 0.999 in
  let ir = (int_of_float (factor *. (clamp r intensity))) in
  let ig = (int_of_float (factor *. (clamp g intensity))) in
  let ib = (int_of_float (factor *. (clamp b intensity))) in
  Printf.fprintf out_channel "%d %d %d\n" ir ig ib
