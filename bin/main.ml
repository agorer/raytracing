module Camera = Raytracing.Camera
module Hittable = Raytracing.Hittable
module Material = Raytracing.Material
module Vec3d = Raytracing.Vec3d
module Color = Raytracing.Color
module Random = Raytracing.Random
open Raytracing.Random

let random_spheres () =
  let spheres = ref [] in
  for a = -11 to 11 do
    for b = -11 to 11 do
      let center = (float_of_int a) +. 0.9 *. random_double(),
                   0.2,
                   (float_of_int b) +. 0.9 *. random_double() in
      let open Vec3d in
      if (length (center - (4.0, 0.2, 0.0))) > 0.9 then
        let material_random = random_double() in
        let material =
          if material_random < 0.8 then
            Material.Lambertian({albedo = (Color.random() ** Color.random())})
          else if material_random < 0.95 then
            Material.Metal({
                albedo = Color.random_beetwen (0.5, 1.0);
                fuzz = (Random.random_between (0.0, 0.5))
              })
          else
            Material.Dielectric({refraction_index = 1.5})
        in
        spheres := Hittable.Sphere({ center; radius = 0.2; material }) :: !spheres
      else ()
    done
  done;
  !spheres

let () =
  let camera = Camera.({
    aspect_ratio = (16. /. 9.);
    image_width = 1200;
    samples_per_pixel = 500;
    max_depth = 50;
    vfov = 20.0;
    lookfrom = 13.0, 2.0, 3.0;
    lookat = 0.0, 0.0, 0.0;
    vup = 0.0, 1.0, 0.0;
    defocus_angle = 0.6;
    focus_dist = 10.0;
  }) in
  let world = [
    Hittable.Sphere({
        center = 0.0, -1000.0, 0.0;
        radius = 1000.0;
        material = Material.Lambertian({albedo = 0.5, 0.5, 0.5}) });
    Hittable.Sphere({
        center = 0.0, 1.0, 0.0;
        radius = 1.0;
        material = Material.Dielectric({refraction_index = 1.5}) });
    Hittable.Sphere({
        center = -4.0, 1.0, 0.0;
        radius = 1.0;
        material = Material.Lambertian({albedo = 0.4, 0.2, 0.1}) });
    Hittable.Sphere({
        center = 4.0, 1.0, 0.0;
        radius = 1.0;
        material = Material.Metal({albedo = 0.7, 0.6, 0.5; fuzz = 0.0}) });
  ] in
  let spheres = random_spheres()  in
  Camera.render camera (world @ spheres)
