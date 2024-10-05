module F = Format
module Color = Raytracing.Color
module Vec3d = Raytracing.Vec3d
module Ray = Raytracing.Ray
module Sphere = Raytracing.Sphere

let aspect_ratio = 16.0 /. 9.0
let image_width = 400
(* NOTE: in theory image_height cannot be less than one *)
let image_height = int_of_float ((float_of_int image_width) /. aspect_ratio)

(* Camera *)
let focal_length = 1.0
let viewport_height = 2.0
let viewport_width =
  viewport_height *. ((float_of_int image_width) /. (float_of_int image_height))
let viewport_u = viewport_width, 0.0, 0.0
let viewport_v = 0.0, (-. viewport_height), 0.0

let pixel_delta_u =
  let open Vec3d in
  viewport_u / (float_of_int image_width)

let pixel_delta_v =
  let open Vec3d in
  viewport_v / (float_of_int image_height)

let camera_center = 0.0, 0.0, 0.0
let viewport_upper_left =
  let open Vec3d in
  camera_center - (0.0, 0.0, focal_length) - (viewport_u / 2.0) - (viewport_v / 2.0)

let pixel00_loc =
  let open Vec3d in
  viewport_upper_left + ((pixel_delta_u + pixel_delta_v) * 0.5)

let ray_color ray =
  let (_, unit_direction_y, _) = Vec3d.unit_vector (Ray.direction ray) in
  let a = 0.5 *. (unit_direction_y +. 1.0) in
  let white = 1.0, 1.0, 1.0 in
  let blue = 0.5, 0.7, 1.0 in
  let open Vec3d in
  if (Sphere.hit_sphere (0.0, 0.0, -1.0) 0.5 ray) then
    1.0, 0.0, 0.0
  else
    (white * (1.0 -. a)) + (blue * a)

let () =
  let oc = open_out "../images/image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;
  for j = 0 to (image_height - 1) do
    F.printf "\rScanlines remaining: %d " (image_height - j);
    for i = 0 to (image_width - 1) do
      let open Vec3d in
      let i = float_of_int i in
      let j = float_of_int j in
      let pixel_center = pixel00_loc + (pixel_delta_u * i) + (pixel_delta_v * j) in
      let ray_direction = pixel_center - camera_center in
      let pixel_color = ray_color (pixel_center, ray_direction) in
      Color.write_color oc pixel_color
    done
  done;
  F.printf "\rDone.                  \n"
