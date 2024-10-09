open Interval
open Vec3d
    
type t = {
  center: Vec3d.t;
  radius: float;
  material: Material.t
}

let hit sphere ray (tmin, tmax) =
  let oc = sphere.center - (Ray.origin ray) in
  let a = Vec3d.dot (Ray.direction ray) (Ray.direction ray) in
  let h = Vec3d.dot oc (Ray.direction ray) in
  let c = (Vec3d.dot oc oc) -. (sphere.radius *. sphere.radius) in
  let discriminant = (h *. h) -. (a *. c) in
  if discriminant < 0.0 then
    None
  else
    let sqrtd = sqrt discriminant in
    let negative_t = (h -. sqrtd) /. a in
    let positive_t = (h +. sqrtd) /. a in
    if between negative_t (tmin, tmax) then
      let point = Ray.at ray negative_t in
      let normal = (point - sphere.center) / sphere.radius in
      Some (Collision.make ray negative_t normal sphere.material)
    else if between positive_t (tmin, tmax) then
      let point = Ray.at ray positive_t in
      let normal = (point - sphere.center) / sphere.radius in
      Some (Collision.make ray positive_t normal sphere.material)
    else
      None
