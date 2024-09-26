type t = Vec3d.t

let write_color out_channel (r, g, b) =
  let ir = (int_of_float (255.999 *. r)) in
  let ig = (int_of_float (255.999 *. g)) in
  let ib = (int_of_float (255.999 *. b)) in
  Printf.fprintf out_channel "%d %d %d\n" ir ig ib
