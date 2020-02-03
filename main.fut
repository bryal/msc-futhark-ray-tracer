import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

module vec2 = mk_vspace_2d f32
type vec2 = vec2.vector

let mkvec2 x y: vec2 = { x, y }
let mkvec3 x y z: vec3 = { x, y, z }

let mkvec3_repeat x: vec3 = { x = x, y = x, z = x }

type ray = { origin: vec3, dir: vec3 }

let mkray (o: vec3) (d: vec3): ray =
  { origin = o, dir = vec3.normalise(d) }
let point_at_param (r: ray) (t: f32): vec3 =
  r.origin vec3.+ vec3.scale t r.dir

module dist = uniform_real_distribution f32 minstd_rand

type maybe 't = #nothing | #just t

type hit = { t: f32, pos: vec3, normal: vec3 }

type sphere = { center: vec3, radius: f32 }
type geom = #sphere sphere
type~ group = []geom

type bounds = { tmin: f32, tmax: f32 }

type rnge = minstd_rand.rng

type camera = { dir: vec3, ratio: f32, origin: vec3 }

let vcol_to_argb (c: vec3): argb.colour =
  argb.from_rgba c.x c.y c.z 1f32

let hit_sphere (bn: bounds) (r: ray) (s: sphere): maybe hit =
  let oc = r.origin vec3.- s.center
  let a = 1 -- vec3.dot r.dir r.dir
  let b = 2 * vec3.dot oc r.dir
  let c = vec3.dot oc oc - s.radius * s.radius
  let discriminant = b * b - 4 * a * c
  let root0 = (-b - f32.sqrt discriminant) / (2 * a)
  let root1 = (-b + f32.sqrt discriminant) / (2 * a)
  let in_bounds t = t < bn.tmax && t > bn.tmin
  let handle_root t =
    let pos = point_at_param r t
    let normal = vec3.scale (1 / s.radius) (pos vec3.- s.center)
    in #just { t, pos, normal }
  in if discriminant > 0
     then if in_bounds root0 then handle_root root0
          else if in_bounds root1 then handle_root root1
          else #nothing
     else #nothing

let hit_geom (bn: bounds) (r: ray) (g: geom): maybe hit =
  match g
  case #sphere s -> hit_sphere bn r s

let hit_group (bn: bounds) (r: ray) (xs: group): maybe hit =
  let select_min_hit a b =
    match (a, b)
    case (#nothing, _) -> b
    case (_, #nothing) -> a
    case (#just a', #just b') -> if a'.t < b'.t then a else b
  in reduce select_min_hit #nothing (map (hit_geom bn r) xs)

let color (r: ray) (world: group): vec3 =
  match hit_group { tmin = 0.0, tmax = f32.highest } r world
  case #just hit' ->
    vec3.scale 0.5 (hit'.normal vec3.+ mkvec3_repeat 1)
  case #nothing ->
    let t = 0.5 * (r.dir.y + 1.0)
    in (vec3.+) (vec3.scale (1.0 - t) (mkvec3_repeat 1))
                (vec3.scale t (mkvec3 0.5 0.7 1.0))

let mk_camera (ratio: f32) (t: f32): camera =
  { dir = mkvec3 0 0 (-1)
  , ratio = ratio
  , origin = mkvec3 (f32.sin (t * 2)) (0.1 + f32.cos t * 0.5) 0.5 }

let to_radians (degs: f32): f32 = degs * f32.pi / 180.0

let get_ray (cam: camera) (coord: vec2): ray =
  let world_up = mkvec3 0 1 0
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
           (screen_bot_left
            vec3.+ vec3.scale coord.x screen_x_dir
            vec3.+ vec3.scale coord.y screen_y_dir)

let sample (w: i32, h: i32)
           (j: i32, i: i32)
           (offset: vec2)
           (t: f32)
         : vec3 =
  let world =
    [ #sphere { center = mkvec3 0 0 (-1), radius = 0.5 }
    , #sphere { center = mkvec3 (-0.5) 0.5 (-2), radius = 0.5 }
    , #sphere { center = mkvec3 8 0 (-10), radius = 0.5 }
    , #sphere { center = mkvec3 0 (-400.4) (-1), radius = 400 } ]
  let wh = mkvec2 (f32.i32 w) (f32.i32 h)
  let ji = mkvec2 (f32.i32 j) (f32.i32 h - f32.i32 i - 1.0)
  let xy = (ji vec2.+ offset) vec2./ wh
  let ratio = f32.i32 w / f32.i32 h
  let cam = mk_camera ratio t
  let r = get_ray cam xy
  in color r world

let sample_all (n: i32)
               (w: i32, h: i32)
               (rng: rnge)
               (time: f32)
             : (rnge, [][]argb.colour) =
  let (rng, offsets) =
    loop (rng, offsets) = (rng, replicate n (mkvec2 0 0))
    for i < n
    do let (rng, offset_x) = dist.rand (0,1) rng
       let (rng, offset_y) = dist.rand (0,1) rng
       in (rng, offsets with [i] = mkvec2 offset_x offset_y)
  let sample' i j offset =
    (vec3./) (sample (w, h) (j, i) offset time)
             (mkvec3_repeat (f32.i32 n))
  let img = tabulate_2d h w <| \i j ->
              vcol_to_argb (reduce_comm (vec3.+)
                                        (mkvec3_repeat 0)
                                        (map (sample' i j) offsets))
  in (rng, img)

module lys: lys with text_content = (i32, i32) = {
  type~ state = { time: f32
                , dimensions: (i32, i32)
                , rng: minstd_rand.rng
                , img: [][]argb.colour
                , samples: i32 }
  let grab_mouse = false

  let init (seed: u32) (h: i32) (w: i32): state =
    { time = 0
    , dimensions = (w, h)
    , rng = minstd_rand.rng_from_seed [123]
    , img = tabulate_2d h w (\_ _ -> argb.black)
    , samples = 1 }

  let resize (h: i32) (w: i32) (s: state) =
    s with dimensions = (w, h)

  let event (e: event) (s: state) =
    match e
      case #step dt ->
        let n = s.samples
        let time = s.time + dt
        let (rng, img) = sample_all n s.dimensions s.rng time
        in s with img = img with rng = rng with time = time
      case #keydown {key} ->
        if key == SDLK_w
        then s with samples = s.samples * 2
        else if key == SDLK_s
        then s with samples =
          if s.samples < 2 then 1 else s.samples / 2
        else s
      case _ -> s

  let render (s: state) = s.img

  let text_format () =
    "FPS: %d\nGOTTA GO FAST\nSAMPLES: %d"

  type text_content = (i32, i32)

  let text_content (render_duration: f32) (s: state): text_content =
      (t32 render_duration, s.samples)

  let text_colour = const argb.yellow
}
