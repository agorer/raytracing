type t = Vec3d.t * Vec3d.t

let origin (origin, _) = origin

let direction (_, direction) = direction

let at (origin, direction) t =
  let open Vec3d in
  origin + (direction * t)
           
