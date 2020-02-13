import "common"

-- TODO: Benchmark if it's faster to represent an aabb as a pair of
-- (top-forward-rightmost corner, bot-backward-leftmost corner).
--
-- Axis Aligned Bounding Box for use with BVHs
type aabb = { center: vec3, height: f32, width: f32, depth: f32 }

let in_bounds (bn: bounds) (t: f32): bool = t < bn.tmax && t > bn.tmin

let hit_triangle (bn: bounds) (ra: ray) (tr: triangle)
                 (mats: []material)
               : maybe hit =
  -- Algorithm from RTR 22.8, variant 22.16, based on 22.8.2
  let eps = 0.00001
  let e1 = tr.b vec3.- tr.a
  let e2 = tr.c vec3.- tr.a
  let n = vec3.cross e1 e2
  let a = -(vec3.dot n ra.dir)
  in if a > -eps && a < eps then #nothing else
  let s = ra.origin vec3.- tr.a
  let m = vec3.cross s ra.dir
  let { x = t, y = u, z = v } =
    vec3.scale (1 / a)
               (mkvec3 (vec3.dot n s)
                       (vec3.dot m e2)
                       (-(vec3.dot m e1)))
  in if u < 0 || v < 0 || u + v > 1 || !(in_bounds bn t)
     then #nothing
     else let pos = point_at_param ra t
          let normal = vec3.normalise n
          in #just { t, pos, normal, mat = unsafe mats[i32.u32 tr.mat_ix] }

let hit_sphere (bn: bounds) (r: ray) (s: sphere)
             : maybe hit =
  let oc = r.origin vec3.- s.center
  let a = 1 -- vec3.dot r.dir r.dir
  let b = 2 * vec3.dot oc r.dir
  let c = vec3.dot oc oc - s.radius * s.radius
  let discriminant = b * b - 4 * a * c
  let root0 = (-b - f32.sqrt discriminant) / (2 * a)
  let root1 = (-b + f32.sqrt discriminant) / (2 * a)
  let handle_root t =
    let pos = point_at_param r t
    let normal = vec3.scale (1 / s.radius) (pos vec3.- s.center)
    in #just { t, pos, normal, mat = s.mat }
  in if discriminant > 0
     then if in_bounds bn root0 then handle_root root0
          else if in_bounds bn root1 then handle_root root1
          else #nothing
     else #nothing

let hit_geom (bn: bounds) (r: ray) (ms: []material) (g: geom)
           : maybe hit =
  match g
  case #sphere s -> hit_sphere bn r s
  case #triangle t -> hit_triangle bn r t ms

let bounding_box_sphere (s: sphere): aabb =
  let diam = s.radius * 2
  in { height = diam, width = diam, depth = diam, center = s.center }

let bounding_box_triangle (t: triangle): aabb =
  let mx = vmax3 t.a t.b t.c
  let mn = vmin3 t.a t.b t.c
  in { center = vec3.scale (1/3) (t.a vec3.+ t.b vec3.+ t.c)
     , height = mx.y - mn.y
     , width = mx.x - mn.x
     , depth = mx.z - mn.z }

let bounding_box_geom (g: geom): aabb =
  match g
  case #sphere s -> bounding_box_sphere s
  case #triangle t -> bounding_box_triangle t
