open Materials

type t =
  | Lambertian of Lambertian.t
  | Metal of Metal.t
  | Dielectric of Dielectric.t

let scatter ray (collision: t Collision.t) =
  match collision.material with
  | Lambertian material ->
    Lambertian.scatter ray { collision with material }
  | Metal material ->
    Metal.scatter ray { collision with material }
  | Dielectric material ->
    Dielectric.scatter ray { collision with material }
