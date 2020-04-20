import "../lib/github.com/diku-dk/cpprandom/random"
import "linalg"


module rnge = minstd_rand
type rnge = rnge.rng

module dist = uniform_real_distribution f32 minstd_rand


let advance_rng (rng: rnge): rnge =
  let (rng, _) = dist.rand (0,1) rng in rng

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

let random_select' [n] 'a (rng: rnge) (xs: [n]a): (rnge, i32, a) =
  let (rng, n) = rnge.rand rng
  let i = i32.u32 (n % u32.i32 (length xs))
  in (rng, i, unsafe xs[i])

let random_select [n] 'a (rng: rnge) (xs: [n]a): (rnge, a) =
  let (rng, _, a) = random_select' rng xs
  in (rng, a)
