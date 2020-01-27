import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

let mkvec3 (x, y, z) : vec3 = {x, y, z}

type ray = { origin: vec3, dir: vec3 }

let mkray (o: vec3) (d: vec3): ray = { origin = o, dir = vec3.normalise(d) }
let point_at_param (r: ray) (t: f32): vec3 = 
  r.origin vec3.+ vec3.scale t r.dir

type maybe 't = #nothing | #just t

type hit = { t: f32, pos: vec3, normal: vec3 }

type geom = #sphere { center: vec3, radius: f32 }
type group = []geom

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

let shoot (w : i32) (h : i32) (j : i32) (i : i32) : argb.colour =
  let world =
    [ #sphere { center = mkvec3 (0, 0, -1), radius = 0.5 }
    , #sphere { center = mkvec3 (0, -100.5, -1), radius = 100 } ]
  let (x, y) = (f32.i32 j / f32.i32 w, (f32.i32 h - f32.i32 i) / f32.i32 h)
  let ratio = f32.i32 w / f32.i32 h
  let bot_left = mkvec3 (-ratio, -1.0, -1.0)
  let vertical = mkvec3 (0.0, 2.0, 0.0)
  let horizontal = mkvec3 (2.0 * ratio, 0.0, 0.0)
  let origin = mkvec3 (0.0, 0.0, 0.0)
  let r = mkray origin (bot_left
                        vec3.+ vec3.scale x horizontal
                        vec3.+ vec3.scale y vertical)
  let col = color r world
  in vcol_to_argb col

module lys: lys with text_content = i32 = {
  type state = {time: f32, h: i32, w: i32}
  let grab_mouse = false

  let init (seed: u32) (h: i32) (w: i32): state =
    {time = 0, w, h}

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h with w = w

  let event (e: event) (s: state) = s

  let render (s: state) =
    tabulate_2d (s.h) (s.w) (\i j -> shoot s.w s.h j i)

  let text_format () =
    "FPS: %d\nGOTTA GO FAST"

  type text_content = i32

  let text_content (render_duration: f32) (s: state): i32 =
      t32 render_duration

  let text_colour = const argb.yellow
}

