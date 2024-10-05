type t = Vec3d.t * float
                     
let hit_sphere center radius ray =
  let open Vec3d in
  let oc = center - (Ray.origin ray) in
  (* let discriminant = (b *. b) -. (4.0 *. a *. c)  in *)
  let a = Vec3d.length_squared (Ray.direction ray) in
  let h = Vec3d.dot (Ray.direction ray) oc in
  let c = (Vec3d.length_squared oc) -. (radius *. radius) in
  let discriminant = (h *. h) -. (a *. c) in
  if discriminant < 0.0 then
    None
  else
    (* In the book we take the other solution to the equation (negative),
       but that makes the sphere not visible 
       maybe we have some rays pointing to the wrong direction? maybe a bug in the book? *)
    Some ((h -. (sqrt discriminant)) /. a)

