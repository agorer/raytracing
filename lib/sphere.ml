let hit_sphere center radius ray =
  let open Vec3d in
  let oc = center - (Ray.origin ray) in
  let a = (Vec3d.dot (Ray.direction ray) (Ray.direction ray)) in
  let b = -2.0 *. (Vec3d.dot (Ray.direction ray) oc) in
  let c = (Vec3d.dot oc oc) -. (radius *. radius) in
  let discriminant = (b *. b) -. (4.0 *. a *. c)  in
  discriminant >= 0.0
