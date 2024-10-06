module F = Format
module Color = Raytracing.Color
module Vec3d = Raytracing.Vec3d
module Ray = Raytracing.Ray
module Sphere = Raytracing.Sphere
module Collision = Raytracing.Collision
module Hittable = Raytracing.Hittable

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

let world = [
  Hittable.Sphere((0., 0., -1.), 0.5);
  Hittable.Sphere((0., -100.5, -1.), 100.);
]

let ray_color ray world =
  let (_, unit_direction_y, _) = Vec3d.unit_vector (Ray.direction ray) in
  let a = 0.5 *. (unit_direction_y +. 1.0) in
  let white = 1.0, 1.0, 1.0 in
  let blue = 0.5, 0.7, 1.0 in
  let open Vec3d in
  let collision = Collision.visible_collision world Hittable.hit ray (0., Float.infinity) in
  match collision with
  | None -> (white * (1.0 -. a)) + (blue * a)
  | Some collision ->
    (collision.normal + (1., 1., 1.)) * 0.5

let () =
  let oc = open_out "../images/image.ppm" in
  let j_limit = image_height - 1 in
  let i_limit = image_width - 1 in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;
  for j = 0 to j_limit do
    for i = 0 to i_limit do
      let open Vec3d in
      let i = float_of_int i in
      let j = float_of_int j in
      let pixel_center = pixel00_loc + (pixel_delta_u * i) + (pixel_delta_v * j) in
      let ray_direction = pixel_center - camera_center in
      let pixel_color = ray_color (camera_center, ray_direction) world in
      Color.write_color oc pixel_color
    done
  done;
