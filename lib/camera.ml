type t = {
  aspect_ratio: float;
  image_width: int;
  samples_per_pixel: int;
  max_depth: int;
  vfov: float;                  (* Vertical view angle (field of view) *)
  lookfrom: float * float * float; 
  lookat: float * float * float;
  vup: float * float * float;   (* Camera relative "up" direction *)
  defocus_angle: float;         (* Variation angle of rays through each pixel *)
  focus_dist: float;            (* Distance from lookfrom point to plane of perfect focus *)
}

type focus = {
  defocus_angle: float;
  defocus_disk_u: Vec3d.t;
  defocus_disk_v: Vec3d.t;
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
      let depth = Stdlib.(depth - 1) in
      let reflexion = Material.scatter ray collision in
      match reflexion with
      | None -> 0.0, 0.0, 0.0
      | Some reflexion ->
        (ray_color reflexion.ray depth world) ** reflexion.attenuation

let sample_square () =
  Random.random_double() -. 0.5, Random.random_double() -. 0.5, 0.0

let defocus_disk_sample center defocus_disk_u defocus_disk_v =
  let open Vec3d in
  let (x, y, _) = random_in_unit_disk() in
  center + (defocus_disk_u * x) + (defocus_disk_v * y)
  
let get_ray i j pixel00_loc delta_u delta_v focus origin =
  let open Vec3d in
  let (x, y, _) = sample_square() in
  let pixel_sample = pixel00_loc +
                     (delta_u * (i +. x)) +
                     (delta_v * (j +. y)) in
  let ray_origin =
    if focus.defocus_angle <= 0.0 then origin
    else defocus_disk_sample origin focus.defocus_disk_u focus.defocus_disk_v in
  let direction = pixel_sample - ray_origin in
  ray_origin, direction

let rec build_color i j world
    pixel00_loc delta_u delta_v focus origin
    samples_left (depth: int) scale color_sum =
  let open Vec3d in
  match samples_left with
  | 0 -> color_sum * scale
  | samples_left ->
    let ray = get_ray i j pixel00_loc delta_u delta_v focus origin in
    let color_sum = color_sum + (ray_color ray depth world) in
    let samples_left = Stdlib.(samples_left - 1) in
    build_color i j world
      pixel00_loc delta_u delta_v focus origin
      samples_left depth scale color_sum

let calculate_viewport_height vfov focus_dist =
  let theta = Geometry.degrees_to_radians vfov in
  let h = Float.tan (theta /. 2.0) in
  2.0 *. h *. focus_dist
  
let render camera world =
  let open Vec3d in
  let oc = open_out "../images/image.ppm" in
  let image_height = int_of_float ((float_of_int camera.image_width) /. camera.aspect_ratio) in
  let viewport_height = calculate_viewport_height camera.vfov camera.focus_dist in
  let viewport_width = viewport_height *.
                       ((float_of_int camera.image_width) /. (float_of_int image_height)) in
  let w = unit_vector (camera.lookfrom - camera.lookat) in
  let u = unit_vector (cross camera.vup w) in
  let v = cross w u in          (* w, u, v are the camera frame basis vectors *)
  let viewport_u = u * viewport_width in
  let viewport_v = (neg v) * viewport_height in
  let viewport_upper_left = camera.lookfrom - (w * camera.focus_dist) -
                            (viewport_u / 2.0) -
                            (viewport_v / 2.0) in
  let pixel_delta_u = viewport_u / (float_of_int camera.image_width) in
  let pixel_delta_v = viewport_v / (float_of_int image_height) in
  let pixel00_loc = viewport_upper_left + ((pixel_delta_u + pixel_delta_v) * 0.5) in
  let pixel_samples_scale = 1.0 /. (float_of_int camera.samples_per_pixel) in
  let defocus_radius = camera.focus_dist *.
                       (Float.tan (Geometry.degrees_to_radians (camera.defocus_angle /. 2.0))) in
  let defocus_disk_u = u * defocus_radius in
  let defocus_disk_v = v * defocus_radius in
  let focus = { defocus_angle = camera.defocus_angle; defocus_disk_u; defocus_disk_v } in
  Printf.fprintf oc "P3\n%d %d\n255\n" camera.image_width image_height;
  for j = 0 to Stdlib.(image_height - 1) do
    for i = 0 to Stdlib.(camera.image_width - 1) do
      let i = float_of_int i in
      let j = float_of_int j in
      let pixel_color =
        build_color i j world
          pixel00_loc pixel_delta_u pixel_delta_v focus camera.lookfrom
          camera.samples_per_pixel camera.max_depth pixel_samples_scale (0., 0., 0.) in
      Color.write_color oc pixel_color
    done
  done;
