type t = {
  aspect_ratio: float;
  image_width: int;
  focal_length: float;
  viewport_height: float;
  center: float * float * float;
  samples_per_pixel: int;
  max_depth: int;
}

let rec ray_color ray (depth: int) world =
  let (_, unit_direction_y, _) = Vec3d.unit_vector (Ray.direction ray) in
  let a = 0.5 *. (unit_direction_y +. 1.0) in
  let white = 1.0, 1.0, 1.0 in
  let blue = 0.5, 0.7, 1.0 in
  let open Vec3d in
  let collision = Collision.visible_collision world Hittable.hit ray (0.001, Float.infinity) in
  match collision with
  | None -> (white * (1.0 -. a)) + (blue * a)
  | Some collision ->
    if depth <= 0 then 0.0, 0.0, 0.0
    else
      let reflexion_direction = Vec3d.random_on_hemisphere collision.normal in
      let depth = Stdlib.(depth - 1) in
      (ray_color (collision.point, reflexion_direction) depth world) * 0.5

let sample_square () =
  Random.random_double() -. 0.5, Random.random_double() -. 0.5, 0.0
  
let get_ray i j pixel00_loc delta_u delta_v origin =
  let open Vec3d in
  let (x, y, _) = sample_square() in
  let pixel_sample = pixel00_loc +
                     (delta_u * (i +. x)) +
                     (delta_v * (j +. y)) in
  let direction = pixel_sample - origin in
  origin, direction

let rec build_color i j world pixel00_loc delta_u delta_v origin samples_left (depth: int) scale color_sum =
  let open Vec3d in
  match samples_left with
  | 0 -> color_sum * scale
  | samples_left ->
    let ray = get_ray i j pixel00_loc delta_u delta_v origin in
    let color_sum = color_sum + (ray_color ray depth world) in
    let samples_left = Stdlib.(samples_left - 1) in
    build_color i j world pixel00_loc delta_u delta_v origin samples_left depth scale color_sum
  
let render camera world =
  let open Vec3d in
  let oc = open_out "../images/image.ppm" in
  let image_height = int_of_float ((float_of_int camera.image_width) /. camera.aspect_ratio) in
  let viewport_width = camera.viewport_height *.
                       ((float_of_int camera.image_width) /. (float_of_int image_height)) in
  let viewport_u = viewport_width, 0.0, 0.0 in
  let viewport_v = 0.0, (-. camera.viewport_height), 0.0 in
  let viewport_upper_left = camera.center -
                            (0.0, 0.0, camera.focal_length) -
                            (viewport_u / 2.0) -
                            (viewport_v / 2.0) in
  let pixel_delta_u = viewport_u / (float_of_int camera.image_width) in
  let pixel_delta_v = viewport_v / (float_of_int image_height) in
  let pixel00_loc = viewport_upper_left + ((pixel_delta_u + pixel_delta_v) * 0.5) in
  let pixel_samples_scale = 1.0 /. (float_of_int camera.samples_per_pixel) in
  Printf.fprintf oc "P3\n%d %d\n255\n" camera.image_width image_height;
  for j = 0 to Stdlib.(image_height - 1) do
    for i = 0 to Stdlib.(camera.image_width - 1) do
      let i = float_of_int i in
      let j = float_of_int j in
      let pixel_color =
        build_color i j world
          pixel00_loc pixel_delta_u pixel_delta_v camera.center
          camera.samples_per_pixel camera.max_depth pixel_samples_scale (0., 0., 0.) in
      Color.write_color oc pixel_color
    done
  done;
