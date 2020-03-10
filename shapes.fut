import "common"

type triangle = { a: vec3, b: vec3, c: vec3 }

type sphere = { center: vec3, radius: f32 }

type geom = #sphere sphere | #triangle triangle

type obj = { geom: geom, mat_ix: u32 }

let mkrect (corners: [4]vec3): [2]geom =
  [ #triangle { a = corners[0]
              , b = corners[1]
              , c = corners[2] }
  , #triangle { a = corners[2]
              , b = corners[3]
              , c = corners[0] } ]

-- TODO: Benchmark if it's faster to represent an aabb as a pair of
-- (top-forward-rightmost corner, bot-backward-leftmost corner).
--
-- Axis Aligned Bounding Box for use with BVHs
type aabb = { center: vec3, half_dims: vec3  }

let in_bounds (t: f32) (tmax: f32): bool = t < tmax && t > 0

type hit' = { t: f32, pos: vec3, normal: vec3 }

let hit_triangle (tmax: f32) (ra: ray) (tr: triangle)
               : maybe hit' =
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
  in if u < 0 || v < 0 || u + v > 1 || !(in_bounds t tmax)
     then #nothing
     else let pos = point_at_param ra t
          let normal = vec3.normalise n
          in #just { t, pos, normal }

let hit_sphere (tmax: f32) (r: ray) (s: sphere)
             : maybe hit' =
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
    in #just { t, pos, normal }
  in if discriminant > 0
     then if in_bounds root0 tmax then handle_root root0
          else if in_bounds root1 tmax then handle_root root1
          else #nothing
     else #nothing

let hit_geom (tmax: f32) (r: ray) (g: geom): maybe hit' =
  match g
  case #sphere s -> hit_sphere tmax r s
  case #triangle t -> hit_triangle tmax r t

let add_mat (mat: material) (h: hit'): hit =
  { t = h.t, pos = h.pos, normal = h.normal, mat }

let hit_obj (tmax: f32) (r: ray) (ms: []material) (obj: obj)
           : maybe hit =
  map_maybe (add_mat (unsafe ms[i32.u32 obj.mat_ix]))
            (hit_geom tmax r obj.geom)

let aabb_min_corner (b: aabb): vec3 =
  b.center vec3.- b.half_dims

let aabb_max_corner (b: aabb): vec3 =
  b.center vec3.+ b.half_dims

let aabb_dimensions (b: aabb): vec3 = vec3.scale 2 b.half_dims

let containing_aabb (b1: aabb) (b2: aabb): aabb =
  let min_corner = vmin (aabb_min_corner b1) (aabb_min_corner b2)
  let max_corner = vmax (aabb_max_corner b1) (aabb_max_corner b2)
  let center = vec3.scale 0.5 (min_corner vec3.+ max_corner)
  in { center
     , half_dims = max_corner vec3.- center }

let bounding_box_point (p: vec3): aabb =
  { center = p, half_dims = mkvec3 0 0 0 }

let bounding_box_sphere (s: sphere): aabb =
  { center = s.center
  , half_dims = mkvec3 s.radius s.radius s.radius }

let bounding_box_triangle (t: triangle): aabb =
  let a = bounding_box_point t.a
  let b = bounding_box_point t.b
  let c = bounding_box_point t.c
  in containing_aabb a (containing_aabb b c)

let bounding_box_geom (g: geom): aabb =
  match g
  case #sphere s -> bounding_box_sphere s
  case #triangle t -> bounding_box_triangle t

let bounding_box_obj: obj -> aabb = bounding_box_geom <-< (.geom)

-- TODO: Also look at
-- http://www.pbr-book.org/3ed-2018/Shapes/Basic_Shape_Interface.html#Bounds3::IntersectP
let hit_aabb (tmax: f32) ({origin, dir}: ray) (b: aabb)
           : bool =
  let eps = 0.001
  let iter min' max' origin' dir' tmin tmax =
    let invD = 1 / dir'
    let t0 = (min' - origin') * invD
    let t1 = (max' - origin') * invD
    let (t0, t1) = if invD < 0 then (t1, t0) else (t0, t1)
    let t1 = t1 * (1 + eps)
    let tmin = f32.max t0 tmin
    let tmax = f32.min t1 tmax
    in (tmin, tmax)
  let (min, max) = (aabb_min_corner b, aabb_max_corner b)
  let (tmin, tmax) =
    iter min.x max.x origin.x dir.x 0 tmax
  in if tmax <= tmin then false
     else let (tmin, tmax) =
            iter min.y max.y origin.y dir.y tmin tmax
          in if tmax <= tmin then false
             else let (tmin, tmax) =
                    iter min.z max.z origin.z dir.z tmin tmax
                  in !(tmax <= tmin)
