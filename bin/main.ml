module Camera = Raytracing.Camera
module Hittable = Raytracing.Hittable
module Material = Raytracing.Material

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
    Hittable.Sphere({
        center = (0.0, -100.5, -1.0);
        radius = 100.0;
        material = Material.Lambertian({albedo = 0.8, 0.8, 0.0}) });
    Hittable.Sphere({
        center = (0., 0., -1.2);
        radius = 0.5;
        material = Material.Lambertian({albedo = 0.1, 0.2, 0.5}) });
    Hittable.Sphere({
        center = (-1.0, 0.0, -1.0);
        radius = 0.5;
        material = Material.Metal({albedo = 0.8, 0.8, 0.8; fuzz = 0.3}) });
    Hittable.Sphere({
        center = (1.0, 0.0, -1.0);
        radius = 0.5;
        material = Material.Metal({albedo = 0.8, 0.6, 0.2; fuzz = 1.0}) });
  ] in
  Camera.render camera world

