type t = {
  albedo: Color.t
}

let scatter _ray (collision: t Collision.t) =
  let open Vec3d in
  let open Reflexion in
  let open Collision in
  let scattered_direction = collision.normal + Vec3d.random_unit_vector() in
  let normalized_direction = 
    if near_zero scattered_direction then collision.normal
    else scattered_direction in
  let scattered = collision.point, normalized_direction in
  Some { ray = scattered; attenuation = collision.material.albedo } 
