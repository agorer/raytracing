type t = {
  albedo: Color.t;
  fuzz: float
}

let scatter ray (collision: t Collision.t) =
  let open Vec3d in
  let reflected = Vec3d.reflect (Ray.direction ray) collision.normal in
  let fuzz = if collision.material.fuzz < 1.0 then collision.material.fuzz else 1.0 in
  let reflected_and_fuzzed = (unit_vector reflected) + (Vec3d.random_unit_vector() * fuzz) in
  let scattered = collision.point, reflected_and_fuzzed in
  if (dot (Ray.direction scattered) collision.normal) > 0.0 then
    Some { Reflexion.ray = scattered; attenuation = collision.material.albedo }
  else
    None
