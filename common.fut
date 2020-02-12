import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"

module rnge = minstd_rand
type rnge = rnge.rng

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

module vec2 = mk_vspace_2d f32
type vec2 = vec2.vector

module dist = uniform_real_distribution f32 minstd_rand

type maybe 't = #nothing | #just t

-- TODO: Don't just supply albedo for red, green, and blue. Handle
-- the whole spectrum somehow!
--
-- TODO: Make hierarchical-like and more physically based
type material =
  { color: vec3
  , fuzz: f32
  , metalness: f32
  , ref_ix: f32
  , emission: vec3 }

type ray = { origin: vec3, dir: vec3 }

type hit = { t: f32, pos: vec3, normal: vec3, mat: material }

type triangle = { a: vec3, b: vec3, c: vec3, mat_ix: u32 }
type sphere = { center: vec3, radius: f32, mat: material }
type geom = #sphere sphere | #triangle triangle
type~ group = []geom

type bounds = { tmin: f32, tmax: f32 }

let mkvec3 x y z: vec3 = { x, y, z }

let mkvec2 x y: vec2 = { x, y }

let mkvec3_repeat x: vec3 = { x = x, y = x, z = x }

let vec3_from_array (xs: [3]f32): vec3 =
  { x = xs[0], y = xs[1], z = xs[2] }

let clamp (min: f32) (max: f32) (x: f32): f32 =
  f32.max min (f32.min max x)

let error_vec: vec3 = mkvec3 1000 0 1000

let random_in_unit_disk (rng: rnge): vec3 =
  let (rng, theta) = dist.rand (0, 2 * f32.pi) rng
  let (_, u) = dist.rand (0, 1) rng
  let r = f32.sqrt u
  in vec3.scale r (mkvec3 (f32.cos theta) (f32.sin theta) 0)

let random_in_unit_sphere (rng: rnge): vec3 =
  let (rng, phi) = dist.rand (0, 2 * f32.pi) rng
  let (rng, costheta) = dist.rand (-1, 1) rng
  let (_, u) = dist.rand (0, 1) rng
  let theta = f32.acos costheta
  let r = u ** (1.0 / 3.0f32)
  let x = f32.sin theta * f32.cos phi
  let y = f32.sin theta * f32.sin phi
  let z = costheta
  in vec3.scale r (mkvec3 x y z)

let world_up: vec3 = mkvec3 0 1 0

let to_radians (degs: f32): f32 = degs * f32.pi / 180.0

let point_at_param (r: ray) (t: f32): vec3 =
  r.origin vec3.+ vec3.scale t r.dir
