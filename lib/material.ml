type t =
  | Lambertian of Materials.Lambertian.t
  | Metal of Materials.Metal.t

let scatter ray (collision: t Collision.t) =
  let open Materials in
  match collision.material with
  | Lambertian material ->
    let collision = { collision with material } in
    Lambertian.scatter ray collision
  | Metal material ->
    let collision = { collision with material } in
    Metal.scatter ray collision
