type t = {
  t: float;
  point: float * float * float;
  normal: float * float * float;
  front_face: bool;
}

(* The parameter outward_normal is assumed to have unit length *)
let set_face_normal colission ray outward_normal =
  let open Vec3d in
  let front_face = dot (Ray.direction ray) outward_normal < 0. in
  let normal = if front_face then outward_normal else neg outward_normal in
  { colission with front_face; normal }

let make ray t normal =
  let point = Ray.at ray t in
  let colission = { t; point; normal; front_face = false; } in
  set_face_normal colission ray normal

let visible_collision objs hit_function ray (tmin, tmax) =
  let select_closest current obj =
    let tmax = match current with
      | None -> tmax
      | Some collision -> collision.t
    in
    let collision = hit_function obj ray (tmin, tmax) in
    match collision with
    | None -> current
    | Some collision -> Some collision
  in
  List.fold_left select_closest None objs
