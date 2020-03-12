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

let is_just 'a (x: maybe a): bool =
  match x case #just _ -> true
          case #nothing -> false

let map_maybe 'a 'b (f: a -> b) (x: maybe a): maybe b =
  match x
  case #just a -> #just (f a)
  case #nothing ->  #nothing

-- TODO: Don't just supply albedo for red, green, and blue. Handle
-- the whole spectrum somehow!
--
-- TODO: Make hierarchical-like and more physically based
type material =
  { color: vec3
  , roughness: f32
  , metalness: f32
  , ref_ix: f32
  , opacity: f32
  , emission: vec3 }

type ray = { origin: vec3, dir: vec3 }

type hit = { t: f32, pos: vec3, normal: vec3, mat: material }

let mkvec3 x y z: vec3 = { x, y, z }

let mkvec2 x y: vec2 = { x, y }

let mkvec3_repeat x: vec3 = { x = x, y = x, z = x }

let vec3_neg ({ x, y, z }: vec3): vec3 = { x = -x, y = -y, z = -z }

let vec3_from_array (xs: [3]f32): vec3 =
  { x = xs[0], y = xs[1], z = xs[2] }

let vec3_lerp (a: vec3) (b: vec3) (r: f32): vec3 =
  vec3.scale (1 - r) a vec3.+ vec3.scale r b

let clamp ((min, max): (f32, f32)) (x: f32): f32 =
  f32.max min (f32.min max x)

let error_vec: vec3 = mkvec3 1000 0 1000

let map_fst 'a 'b 'c (f: a -> c) (x: a, y: b): (c, b) = (f x, y)
let map_snd 'a 'b 'c (f: b -> c) (x: a, y: b): (a, c) = (x, f y)

-- [0, 1)
let random_unit_exclusive (rng: rnge): (rnge, f32) =
  dist.rand (0, 0.9999) rng

-- Random sample a point in the unit disk.
--
-- Range is exclusive, i.e. radius < 1 => |v| < 1.
let random_in_unit_disk (rng: rnge): (rnge, vec3) =
  let (rng, theta) = dist.rand (0, 2 * f32.pi) rng
  let (rng, u) = random_unit_exclusive rng
  let r = f32.sqrt u
  in (rng, vec3.scale r (mkvec3 (f32.cos theta) (f32.sin theta) 0))

-- [0, 1)^2
let random_in_unit_square (rng: rnge): (rnge, (f32, f32)) =
  let (rng, x) = random_unit_exclusive rng
  let (rng, y) = random_unit_exclusive rng
  in (rng, (x, y))

let world_up: vec3 = mkvec3 0 1 0

let to_radians (degs: f32): f32 = degs * f32.pi / 180.0

let point_at_param (r: ray) (t: f32): vec3 =
  r.origin vec3.+ vec3.scale t r.dir

let inv_pi: f32 = 1.0 / f32.pi

let vmax (u: vec3) (v: vec3): vec3 =
  vec3.map2 f32.max u v

let vmax3 (u: vec3) (v: vec3) (w: vec3): vec3 =
  vmax u (vmax v w)

let vmin (u: vec3) (v: vec3): vec3 =
  vec3.map2 f32.min u v

let vmin3 (u: vec3) (v: vec3) (w: vec3): vec3 =
  vmin u (vmin v w)
