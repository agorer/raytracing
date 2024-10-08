module Camera = Raytracing.Camera
module Hittable = Raytracing.Hittable

let () =
  let camera = Camera.({
    aspect_ratio = (16. /. 9.);
    image_width = 400;
    focal_length = 1.;
    viewport_height = 2.;
    center = 0., 0., 0.;
    samples_per_pixel = 100;
    max_depth = 50;
  }) in
  let world = [
    Hittable.Sphere((0., 0., -1.), 0.5);
    Hittable.Sphere((0., -100.5, -1.), 100.);
  ] in
  Camera.render camera world

