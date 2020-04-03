import "../lib/github.com/athas/vector/vspace"


module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

module vec2 = mk_vspace_2d f32
type vec2 = vec2.vector

type angle = { radians: f32 }


let mkvec3 x y z: vec3 = { x, y, z }

let mkvec2 x y: vec2 = { x, y }

let mkvec3_repeat x: vec3 = { x = x, y = x, z = x }

let vec3_neg ({ x, y, z }: vec3): vec3 = { x = -x, y = -y, z = -z }

let vec3_from_array (xs: [3]f32): vec3 =
  { x = xs[0], y = xs[1], z = xs[2] }

let vec3_lerp (a: vec3) (b: vec3) (r: f32): vec3 =
  vec3.scale (1 - r) a vec3.+ vec3.scale r b

-- If necessary, flip `w` around to face the same side as `dominant`.
let same_side (dominant: vec3) (w: vec3): vec3 =
  vec3.scale (f32.sgn (vec3.dot dominant w)) w

let error_vec: vec3 = mkvec3 1000 0 1000

let vmax (u: vec3) (v: vec3): vec3 =
  vec3.map2 f32.max u v

let vmax3 (u: vec3) (v: vec3) (w: vec3): vec3 =
  vmax u (vmax v w)

let vmin (u: vec3) (v: vec3): vec3 =
  vec3.map2 f32.min u v

let vmin3 (u: vec3) (v: vec3) (w: vec3): vec3 =
  vmin u (vmin v w)

let world_up: vec3 = mkvec3 0 1 0

let to_rad (a: angle): f32 = a.radians
let from_rad (r: f32): angle = { radians = r }

let to_deg (a: angle): f32 = a.radians * 180.0 / f32.pi
let from_deg (d: f32): angle = { radians = d * f32.pi / 180.0 }

let inv_pi: f32 = 1.0 / f32.pi
