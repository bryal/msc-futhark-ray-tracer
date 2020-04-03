import "common"
import "linalg"
import "camera"

type ray = { origin: vec3, dir: vec3 }

type hit = { t: f32, pos: vec3, normal: vec3 }

type triangle = { a: vec3, b: vec3, c: vec3 }
type sphere = { center: vec3, radius: f32 }

type geom = #sphere sphere | #triangle triangle

-- TODO: Benchmark if it's faster to represent an aabb as a pair of
-- (top-forward-rightmost corner, bot-backward-leftmost corner).
--
-- Axis Aligned Bounding Box for use with BVHs
type aabb = { center: vec3, half_dims: vec3  }


let mkray (o: vec3) (d: vec3): ray =
  { origin = o, dir = vec3.normalise(d) }

-- Create a ray from a point in a direction, fix for surface acne
let mkray_adjust_acne (h: hit) (wi: vec3): ray =
  -- Note that we don't just walk along `wi`, because then we get
  -- strange artifacts for some materials at extreme angles.
  let eps = 0.001
  let acne_offset = vec3.scale eps (same_side wi h.normal)
  in mkray (h.pos vec3.+ acne_offset) wi

let point_at_param (r: ray) (t: f32): vec3 =
  r.origin vec3.+ vec3.scale t r.dir

let get_ray (cam: camera) (ratio: f32) (coord: vec2) (rng: rnge): ray =
  let lens_radius = cam.aperture / 2
  let field_of_view = from_deg 80.0
  let half_height = f32.tan ((to_rad field_of_view) / 2.0)
  let half_width = ratio * half_height
  let (w, u, v) =
    (vec3.scale (-1) (cam_dir cam), cam_right cam, cam_up cam)
  let focus_dist = cam.focal_dist
  let lower_left_corner =
    cam.origin
    vec3.- vec3.scale (half_width * focus_dist) u
    vec3.- vec3.scale (half_height * focus_dist) v
    vec3.- vec3.scale focus_dist w
  let horizontal = vec3.scale (2 * half_width * focus_dist) u
  let vertical = vec3.scale (2 * half_height * focus_dist) v
  let (_, d) = random_in_unit_disk rng
  let lens = vec3.scale lens_radius d
  let lens_offset = vec3.scale lens.x u vec3.+ vec3.scale lens.y v
  let origin = cam.origin vec3.+ lens_offset
  in mkray origin
           (lower_left_corner
            vec3.+ vec3.scale coord.x horizontal
            vec3.+ vec3.scale coord.y vertical
            vec3.- origin)

let mkrect (corners: [4]vec3): [2]geom =
  [ #triangle { a = corners[0]
              , b = corners[1]
              , c = corners[2] }
  , #triangle { a = corners[2]
              , b = corners[3]
              , c = corners[0] } ]

let triangle_normal (t: triangle): vec3 =
  let e1 = t.b vec3.- t.a
  let e2 = t.c vec3.- t.a
  in vec3.normalise (vec3.cross e1 e2)

let in_bounds (t: f32) (tmax: f32): bool = t < tmax && t > 0

let hit_triangle (tmax: f32) (ra: ray) (tr: triangle)
               : maybe hit =
  -- Algorithm from RTR 22.8, variant 22.16, based on 22.8.2
  let eps = 0.00001
  let e1 = tr.b vec3.- tr.a
  let e2 = tr.c vec3.- tr.a
  let n = vec3.cross e1 e2
  let a = -(vec3.dot n ra.dir)
  in maybe.when (!(approx_zero a eps))
     <| let s = ra.origin vec3.- tr.a
        let m = vec3.cross s ra.dir
        let { x = t, y = u, z = v } =
          vec3.scale (1 / a)
                     (mkvec3 (vec3.dot n s)
                             (vec3.dot m e2)
                             (-(vec3.dot m e1)))
        let in_triangle = u >= 0 && v >= 0 && u + v <= 1
        in maybe.guard (in_triangle && in_bounds t tmax)
           <| let pos = point_at_param ra t
              let normal = vec3.normalise n
              in { t, pos, normal }

let hit_sphere (tmax: f32) (r: ray) (s: sphere)
             : maybe hit =
  let oc = r.origin vec3.- s.center
  let b = 2 * vec3.dot oc r.dir
  let c = vec3.dot oc oc - s.radius * s.radius
  let discriminant = b * b - 4 * c
  let root0 = (-b - f32.sqrt discriminant) / 2
  let root1 = (-b + f32.sqrt discriminant) / 2
  let handle_root t =
    let pos = point_at_param r t
    let normal = vec3.scale (1 / s.radius) (pos vec3.- s.center)
    in { t, pos, normal }
  in maybe.when (discriminant > 0)
     <| maybe.or (maybe.guard (in_bounds root0 tmax) (handle_root root0))
                 (maybe.guard (in_bounds root1 tmax) (handle_root root1))

let hit_geom (tmax: f32) (r: ray) (g: geom): maybe hit =
  match g
  case #sphere s -> hit_sphere tmax r s
  case #triangle t -> hit_triangle tmax r t

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
