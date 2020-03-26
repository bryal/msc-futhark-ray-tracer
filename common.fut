import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"

module rnge = minstd_rand
type rnge = rnge.rng

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

module vec2 = mk_vspace_2d f32
type vec2 = vec2.vector

module dist = uniform_real_distribution f32 minstd_rand

type^ lazy 't = () -> t

let strict 't (x: lazy t): t = x ()

module maybe = {
  type maybe 't = #nothing | #just t

  let is_just 'a (x: maybe a): bool =
    match x case #just _ -> true
            case #nothing -> false

  let map 'a 'b (f: a -> b) (x: maybe a): maybe b =
    match x
    case #just a -> #just (f a)
    case #nothing ->  #nothing

  let when 'a (pred: bool) (x: maybe a): maybe a =
    if pred then x else #nothing

  -- Non-strict version of `when`
  let when' 'a (pred: bool) (x: () -> maybe a): maybe a =
    if pred then strict x else #nothing

  let guard 'a (pred: bool) (x: a): maybe a =
    if pred then #just x else #nothing

  let or 'a (x: maybe a) (y: maybe a): maybe a =
    match x
    case #just a -> #just a
    case #nothing -> y
}

type maybe 't = maybe.maybe t

let approx_zero (a: f32) (eps: f32): bool = a > -eps && a < eps

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

type spectrum = { b0: (f32, f32)
                , b1: (f32, f32)
                , b2: (f32, f32)
                , b3: (f32, f32)
                , b4: (f32, f32)
                , b5: (f32, f32) }

let spectrum_to_arr (s: spectrum) : [6](f32, f32) =
  [ ((s.b0).0, (s.b0).1)
  , ((s.b1).0, (s.b1).1)
  , ((s.b2).0, (s.b2).1)
  , ((s.b3).0, (s.b3).1)
  , ((s.b4).0, (s.b4).1)
  , ((s.b5).0, (s.b5).1) ]

let spectrum_lookup (v: f32) (s: spectrum) : f32 =
  -- TODO: SOACs don't seem to work well here. Compiler bug? Seems it
  --       can't compute size of memory to allocate before kernel
  --       start. Anywho, that's why we're not using `filter` etc.
  let ((w_below, x_below), (w_above, x_above)) =
    loop ((w_below, x_below), (w_above, x_above)) = ((-1, 0), (f32.inf, 0))
    for (w, x) in spectrum_to_arr s
    do if w > w_below && w <= v
       then ((w, x), (w_above, x_above))
       else if w < w_above && w > v
       then ((w_below, x_below), (w, x))
       else ((w_below, x_below), (w_above, x_above))
  in match (w_below < 0, f32.isinf w_above)
     case (true, true) -> 0
     case (true, false) -> x_above
     case (false, true) -> x_below
     case (false, false) ->
          f32.lerp x_below
                   x_above
                   ((v - w_below) / (w_above - w_below))

type material =
  { color: spectrum
  , roughness: f32
  , metalness: f32
  , ref_ix: f32
  , opacity: f32
  , emission: spectrum }

type ray = { origin: vec3, dir: vec3 }

type hit = { t: f32, pos: vec3, normal: vec3, mat: material }

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

-- PBR Book 13.6.5
let random_in_triangle (rng: rnge): (rnge, (f32, f32)) =
  let (rng, (u, v)) = random_in_unit_square rng
  let su = f32.sqrt u
  in (rng, (1 - su, v * su))

let random_select [n] 'a (rng: rnge) (xs: [n]a): (rnge, a) =
  let (rng, n) = rnge.rand rng
  in (rng, unsafe xs[i32.u32 (n % u32.i32 (length xs))])

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
