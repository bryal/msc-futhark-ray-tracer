import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

let mkvec3 (x, y, z) : vec3 = {x, y, z}

type ray = { origin: vec3, dir: vec3 }

let mkray (o: vec3) (d: vec3): ray = { origin = o, dir = vec3.normalise(d) }
let point_at_param (r: ray) (t: f32): vec3 = 
  r.origin vec3.+ vec3.scale t r.dir

module dist = uniform_real_distribution f32 minstd_rand

type maybe 't = #nothing | #just t

type hit = { t: f32, pos: vec3, normal: vec3 }

type geom = #sphere { center: vec3, radius: f32 }
type~ group = []geom

let vcol_to_argb (c : vec3) : argb.colour = argb.from_rgba c.x c.y c.z 1f32

let hit_sphere (tmin: f32) (tmax: f32) (r: ray) (center: vec3) (radius: f32): maybe hit =
  let oc = r.origin vec3.- center
  let a = 1 -- vec3.dot r.dir r.dir
  let b = 2 * vec3.dot oc r.dir
  let c = vec3.dot oc oc - radius * radius
  let discriminant = b * b - 4 * a * c
  in if discriminant > 0
     then let t = (-b - f32.sqrt discriminant) / (2 * a)
          in if t < tmax && t > tmin
             then let pos = point_at_param r t
                  let normal = vec3.scale (1/radius) (pos vec3.- center)
                  in #just { t, pos, normal }
             else let t = (-b + f32.sqrt discriminant) / (2 * a)
                  in if t < tmax && t > tmin
                    then let pos = point_at_param r t
                         let normal = vec3.scale (1/radius) (pos vec3.- center)
                         in #just { t, pos, normal }
                    else #nothing
     else #nothing 

let hit_geom (tmin: f32) (tmax: f32) (r: ray) (g: geom): maybe hit =
  match g
  case #sphere { center, radius } -> hit_sphere tmin tmax r center radius

let hit_group (tmin: f32) (tmax: f32) (r: ray) (xs: group): maybe hit =
  let select_min_hit a b =
    match (a, b)
    case (#nothing, _) -> b
    case (_, #nothing) -> a
    case (#just a', #just b') -> if a'.t < b'.t then a else b
  in reduce select_min_hit #nothing (map (hit_geom tmin tmax r) xs)

let color (r: ray) (world: group): vec3 =
  match hit_group 0.0 f32.highest r world
  case #just hit' -> 
    vec3.scale 0.5 (hit'.normal vec3.+ mkvec3 (1, 1, 1))
  case #nothing -> 
    let t = 0.5 * (r.dir.y + 1.0)
    in (vec3.+) (vec3.scale (1.0 - t) (mkvec3 (1.0, 1.0, 1.0)))
                (vec3.scale t (mkvec3 (0.5, 0.7, 1.0)))

type camera = { dir: vec3, ratio: f32, origin: vec3 }

let mk_camera (ratio: f32) (t: f32): camera =
  { dir = mkvec3 (0, 0, -1)
  , ratio = ratio
  , origin = mkvec3 (f32.sin (t * 2), 0.1 + f32.cos t * 0.5, 0.5) }

let to_radians (degs: f32): f32 = degs * f32.pi / 180.0

let get_ray (cam: camera) (x: f32) (y: f32): ray =
  let world_up = mkvec3 (0, 1, 0)
  let field_of_view = 80.0
  let f = (to_radians field_of_view) / 2.0
  let cam_right = vec3.normalise (vec3.cross cam.dir world_up)
  let cam_up = vec3.normalise (vec3.cross cam_right cam.dir)
  let a = vec3.scale (f32.cos f) cam.dir
  let b = vec3.scale (f32.sin f) cam_up
  let c = vec3.scale (f32.sin f * cam.ratio) cam_right
  let screen_bot_left = a vec3.- c vec3.- b
  let screen_x_dir = vec3.scale 2 (a vec3.- b vec3.- screen_bot_left)
  let screen_y_dir = vec3.scale 2 (a vec3.- c vec3.- screen_bot_left)
  in mkray cam.origin
           (       screen_bot_left
            vec3.+ vec3.scale x screen_x_dir
            vec3.+ vec3.scale y screen_y_dir)

type rnge = minstd_rand.rng

let sample (w: i32, h: i32) (j : i32, i : i32) (offset_x : f32, offset_y : f32) (t: f32) : vec3 =
  let world =
    [ #sphere { center = mkvec3 (0, 0, -1), radius = 0.5 }
    , #sphere { center = mkvec3 (-0.5, 0.5, -2), radius = 0.5 }
    , #sphere { center = mkvec3 (8, 0, -10), radius = 0.5 }
    , #sphere { center = mkvec3 (0, -400.4, -1), radius = 400 } ]
  let j = f32.i32 j + offset_x
  let i = f32.i32 i + offset_y
  let (x, y) = (j / f32.i32 w, (f32.i32 h - i) / f32.i32 h)
  let ratio = f32.i32 w / f32.i32 h
  let cam = mk_camera ratio t
  let r = get_ray cam x y
  in color r world

let sample_all (n : i32) (w : i32) (h : i32) (rng : rnge) (time: f32) : (rnge, [][]argb.colour) =
	let (rng, offsets) =
		loop (rng, offsets) = (rng, replicate n (0,0))
		  for i < n
				do let (rng, offset_x) = dist.rand (0,1) rng
        	let (rng, offset_y) = dist.rand (0,1) rng
					in (rng, offsets with [i] = (offset_x, offset_y))
  let sample' i j offset = sample (w, h) (j, i) offset time vec3./ mkvec3(f32.i32 n, f32.i32 n, f32.i32 n)
	let img = tabulate_2d h w (\i j -> vcol_to_argb (reduce_comm (vec3.+) (mkvec3 (0.0, 0, 0)) (map (sample' i j) offsets)))
  in (rng, img)

module lys: lys with text_content = i32 = {
  type~ state = {time: f32, h: i32, w: i32, rng: minstd_rand.rng, img: [][]argb.colour}
  let grab_mouse = false

  let init (seed: u32) (h: i32) (w: i32): state =
    { time = 0
		, w
		, h
		, rng = minstd_rand.rng_from_seed [123]
		, img = tabulate_2d h w (\_ _ -> argb.black)}

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h with w = w

  let event (e: event) (s: state) =
		match e
			case #step dt ->
        let n = 100
        let time = s.time + dt
        let (rng, img) = sample_all n s.w s.h s.rng time
				in s with img = img with rng = rng with time = time
			case _     -> s

  let render (s: state) = s.img

  let text_format () =
    "FPS: %d\nGOTTA GO FAST"

  type text_content = i32

  let text_content (render_duration: f32) (s: state): i32 =
      t32 render_duration

  let text_colour = const argb.yellow
}
