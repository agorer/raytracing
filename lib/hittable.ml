type hittable =
  | Sphere of Sphere.t

let hit obj ray limits =
  match obj with
  | Sphere data -> Sphere.hit data ray limits
