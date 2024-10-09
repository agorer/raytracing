type t = {
  (* Refractive index in vacuum or air, or the ratio of the material's
     refractive index overthe refractive index of the enclosing media *)
  refraction_index: float
}

let reflectance cos refraction_index =
  (* Schlick's approximation for reflectance *)
  let r0 = (1.0 -. refraction_index) /. (1.0 +. refraction_index) in
  let r0 = r0 *. r0 in
  r0 +. (1.0 -. r0) *. (Float.pow (1.0 -. cos) 5.0)

let scatter ray (collision: t Collision.t) =
  let open Reflexion in
  let ri =
    if collision.front_face then 1.0 /. collision.material.refraction_index
    else collision.material.refraction_index in
  let unit_direction = Vec3d.unit_vector (Ray.direction ray) in
  let cos_theta =
    Float.min (Vec3d.dot (Vec3d.neg unit_direction) collision.normal) 1.0 in
  let sin_theta = Float.sqrt (1.0 -. (cos_theta *. cos_theta)) in
  let direction =
    if (ri *. sin_theta) > 1.0 ||
       (reflectance cos_theta ri) > Random.random_double() then
      Vec3d.reflect unit_direction collision.normal
    else
      Vec3d.refract unit_direction collision.normal ri in
  Some { ray = (collision.point, direction); attenuation = 1.0, 1.0, 1.0 } 
