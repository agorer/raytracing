type t = {
  aspect_ratio: float;
  image_width: int;
  focal_length: float;
  viewport_height: float;
  center: float * float * float;
}

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
  
let render camera world =
  let open Vec3d in
  let oc = open_out "../images/image.ppm" in
  let image_height = int_of_float ((float_of_int camera.image_width) /. camera.aspect_ratio) in
  let viewport_width = camera.viewport_height *. ((float_of_int camera.image_width) /. (float_of_int image_height)) in
  let viewport_u = viewport_width, 0.0, 0.0 in
  let viewport_v = 0.0, (-. camera.viewport_height), 0.0 in
  let viewport_upper_left = camera.center - (0.0, 0.0, camera.focal_length) - (viewport_u / 2.0) - (viewport_v / 2.0) in
  let pixel_delta_u = viewport_u / (float_of_int camera.image_width) in
  let pixel_delta_v = viewport_v / (float_of_int image_height) in
  let pixel00_loc = viewport_upper_left + ((pixel_delta_u + pixel_delta_v) * 0.5) in
  Printf.fprintf oc "P3\n%d %d\n255\n" camera.image_width image_height;
  for j = 0 to Stdlib.(image_height - 1) do
    for i = 0 to Stdlib.(camera.image_width - 1) do
      let open Vec3d in
      let i = float_of_int i in
      let j = float_of_int j in
      let pixel_center = pixel00_loc + (pixel_delta_u * i) + (pixel_delta_v * j) in
      let ray_direction = pixel_center - camera.center in
      let pixel_color = ray_color (camera.center, ray_direction) world in
      Color.write_color oc pixel_color
    done
  done;
