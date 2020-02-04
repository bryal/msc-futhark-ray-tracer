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

-- TODO: Don't just supply albedo for red, green, and blue. Handle
-- the whole spectrum somehow!
--
-- Albedo ≈ Color
type material
  = #metal { albedo: vec3, fuzz: f32 }
  | #lambertian { albedo: vec3 }
  | #dielectric { ref_ix: f32 }

type hit = { t: f32, pos: vec3, normal: vec3, mat: material }

type sphere = { center: vec3, radius: f32, mat: material }
type geom = #sphere sphere
type~ group = []geom

type bounds = { tmin: f32, tmax: f32 }

module rnge = minstd_rand
type rnge = rnge.rng

type camera = { dir: vec3, origin: vec3 }

let vcol_to_argb (c: vec3): argb.colour =
  argb.from_rgba c.x c.y c.z 1f32

-- TODO: Improve. Do this in deterministic constant time.
let random_in_unit_sphere (rng: rnge): vec3 =
  let (v, _, _) =
    -- NOTE: This `n` prevents us from getting stuck in seemingly
    -- infinite loops. Use a better algo.
    loop (p, rng, n) = (mkvec3 1 1 1, rng, 4u32)
    while vec3.quadrance p >= 1 && n > 0
    do let (rng, x) = dist.rand (0,1) rng
       let (rng, y) = dist.rand (0,1) rng
       let (rng, z) = dist.rand (0,1) rng
       let v = vec3.scale 2 (mkvec3 x y z) vec3.- mkvec3 1 1 1
       in (v, rng, n - 1)
  in v

-- TODO: Glossy reflections. Check out wikipedia/Schlick's
-- approximation. Use halfway vector? Rest of the microfacet model.
let schlick (wi: vec3) (normal: vec3) (n1: f32) (n2: f32): f32 =
  let r0 = let x = (n1 - n2) / (n1 + n2) in x * x
  in r0 + (1 - r0) * (1 + vec3.dot normal wi)**5

let reflect (wi: vec3) (n: vec3): vec3 =
  wi vec3.- vec3.scale (2 * vec3.dot wi n) n

-- Returns #nothing on total internal reflection
--
-- We use the (apparently, we have not benchmarked it) efficient
-- method of Bec presented in Real Time Rendering formula 14.28
-- (p. 627) to compute the refraction vector. This is an
-- implementation of Snell's Law, not an approximation.
--
-- About `discriminant`: "Total internal reflection is indicated by a
-- negative radiccand in the equation for cos(θ₂)" according to
-- wikipedia/Snell's Law. k = cos(θ₂) in our function.
let refract (wi: vec3) (n: vec3) (relative_ix: f32): maybe vec3 =
  let l = vec3.scale (-1) wi
  let w = relative_ix * vec3.dot l n
  let discriminant = 1 + (w - relative_ix) * (w + relative_ix)
  in if discriminant < 0
     then #nothing
     else let k = f32.sqrt discriminant
          let t = vec3.scale (w - k) n vec3.- vec3.scale relative_ix l
          in #just t

-- Transmittance: The amount of light not absorbed by the hit object
--
-- NOTE: What we call transmittance here is called attenuation in
-- RTi1W, but that seemed inversed to us.
--
-- NOTE: In some literature, wi and wo are reversed, so wi is the
-- direction for the incoming light from the sun, and wo is the
-- outgoing vector towards the camera
let scatter (wi: vec3) (h: hit) (rng: rnge)
          : { transmit: vec3, wo: vec3 } =
  match h.mat
  case #metal { fuzz, albedo } ->
    let reflected = reflect wi h.normal
    let scatter_sphere =
      vec3.scale fuzz (random_in_unit_sphere rng)
    let wo = vec3.normalise (reflected vec3.+ scatter_sphere)
    -- If we hit a fuzzy metal sphere close to the edge, i.e. tangent
    -- / almost tangent it, the scatter sphere we "create" may
    -- intersect the metal sphere, and we may sample a wo that is
    -- actually pointing inwards. We're not sure what the real-life
    -- equivalence is, but as the RTi1W did, we simply return black in
    -- those cases, treating it as if the sphere absorbed the ray.
    let transmit =
      if vec3.dot wo h.normal > 0 then albedo else mkvec3 0 0 0
    in { transmit, wo }
  case #lambertian { albedo } ->
    let target = h.pos vec3.+ h.normal
                              vec3.+ random_in_unit_sphere rng
    let wo = vec3.normalise (target vec3.- h.pos)
    in { transmit = albedo, wo }
  case #dielectric { ref_ix } ->
    let (outward_normal, n1, n2) =
      if vec3.dot wi h.normal > 0
      then (vec3.scale (-1) h.normal, ref_ix, 1.0)
      else (h.normal, 1.0, ref_ix)
    let relative_ix = n1 / n2
    let (_, russian_roulette) = dist.rand (0,1) rng
    -- TODO: Do we really need a nothing case here? Seems like schlick
    -- could immediately tell us if there's total internal reflection.
    let wo =
      match refract wi outward_normal relative_ix
      case #just refracted ->
        if russian_roulette >= schlick wi outward_normal n1 n2
        then refracted
        else reflect wi h.normal
      case #nothing -> reflect wi h.normal
    in { transmit = mkvec3 1 1 1, wo }

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
    in #just { t, pos, normal, mat = s.mat }
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

let advance_rng (rng: rnge): rnge =
  let (rng, _) = dist.rand (0,1) rng in rng

let color (r: ray) (world: group) (rng: rnge): vec3 =
  let bounds = { tmin = 0.0, tmax = f32.highest }
  let sky = mkvec3 0.8 0.9 1.0
  let (c, _, _, _) =
    loop (throughput, r, rng, bounces) =
         (mkvec3 1 1 1, r, rng, 8u32)
    while bounces > 0 && vec3.norm throughput > 0.01
    do match hit_group bounds r world
       case #just hit' ->
         let { transmit, wo } = scatter r.dir hit' rng
         let rng = advance_rng rng
         let throughput = transmit vec3.* throughput
         let eps = 0.001
         let side = if vec3.dot wo hit'.normal >= 0 then 1 else (-1)
         -- Fix surface acne
         --
         -- NOTE: Don't just walk along `wo`, because then we get
         -- strange artifacts for some materials at extreme angles.
         let acne_offset = vec3.scale (side * eps) hit'.normal
         let r = mkray (hit'.pos vec3.+ acne_offset) wo
         in (throughput, r, rng, bounces - 1)
       case #nothing ->
         (throughput vec3.* sky, r, rng, 0)
  in c

let to_radians (degs: f32): f32 = degs * f32.pi / 180.0

let world_up: vec3 = mkvec3 0 1 0

let cam_right (cam: camera): vec3 =
  vec3.normalise (vec3.cross cam.dir world_up)

let cam_up (cam: camera): vec3 =
  vec3.normalise (vec3.cross (cam_right cam) cam.dir)

let get_ray (cam: camera) (ratio: f32) (coord: vec2): ray =
  let field_of_view = 80.0
  let f = (to_radians field_of_view) / 2.0
  let a = vec3.scale (f32.cos f) cam.dir
  let b = vec3.scale (f32.sin f) (cam_up cam)
  let c = vec3.scale (f32.sin f * ratio) (cam_right cam)
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
           (rng: rnge)
           (cam: camera)
         : vec3 =
  let world =
    [ #sphere { center = mkvec3 1.6 0 (-0.8)
    	        , radius = 0.5
    	        , mat = #lambertian { albedo = mkvec3 1 1 1 } }
    , #sphere { center = mkvec3 0 0 (-1.5)
    	        , radius = 0.5
    	        , mat = #metal { albedo = mkvec3 1 0.6 0, fuzz = 0.6 } }
    , #sphere { center = mkvec3 0.4 0.2 (-0.6)
    	        , radius = 0.5
    	        , mat = #dielectric { ref_ix = 1.6 } }
    , #sphere { center = mkvec3 0 (-400.4) (-1)
    	        , radius = 400
      	      , mat = #lambertian { albedo = mkvec3 0.2 0.8 0.3 }
              } ]
  let wh = mkvec2 (f32.i32 w) (f32.i32 h)
  let ji = mkvec2 (f32.i32 j) (f32.i32 h - f32.i32 i - 1.0)
  let xy = (ji vec2.+ offset) vec2./ wh
  let ratio = f32.i32 w / f32.i32 h
  let r = get_ray cam ratio xy
  in color r world rng

let sample_all (n: i32)
               (w: i32, h: i32)
               (rng: rnge)
               (cam: camera)
             : (rnge, [][]argb.colour) =
  let rngs = rnge.split_rng n rng
  let rngss = map (rnge.split_rng (w * h)) rngs
  let sample' i j rngs =
    let ix = i * w + j
    let rng = rngs[ix]
    let (rng, offset_x) = dist.rand (0,1) rng
    let (rng, offset_y) = dist.rand (0,1) rng
    let offset = mkvec2 offset_x offset_y
    in (vec3./) (sample (w, h) (j, i) offset rng cam)
                (mkvec3_repeat (f32.i32 n))
  let img = tabulate_2d h w <| \i j ->
              vcol_to_argb (reduce_comm
                              (vec3.+)
                              (mkvec3_repeat 0)
                              (map (sample' i j) rngss))
  in (advance_rng rng, img)

let move_camera (cam: camera) (m: vec3): camera =
  let axes = cam_up cam vec3.+ cam_right cam vec3.+ cam.dir
  in cam with origin = cam.origin vec3.+ vec3.scale 0.1 m vec3.* axes

module lys: lys with text_content = (i32, i32) = {
  type~ state = { time: f32
                , dimensions: (i32, i32)
                , rng: minstd_rand.rng
                , img: [][]argb.colour
                , samples: i32
                , cam: camera }
  let grab_mouse = false

  let init (seed: u32) (h: i32) (w: i32): state =
    { time = 0
    , dimensions = (w, h)
    , rng = minstd_rand.rng_from_seed [123]
    , img = tabulate_2d h w (\_ _ -> argb.black)
    , samples = 1
    , cam = { dir = mkvec3 0 0 (-1)
            , origin = mkvec3 0 0.1 0.5 }}

  let resize (h: i32) (w: i32) (s: state) =
    s with dimensions = (w, h)

  let event (e: event) (s: state) =
    match e
      case #step dt ->
        let n = s.samples
        let time = s.time + dt
        let (rng, img) = sample_all n s.dimensions s.rng s.cam
        in s with img = img with rng = rng with time = time
      case #keydown {key} ->
        if key == SDLK_UP
        then s with samples = s.samples * 2
        else if key == SDLK_DOWN
        then s with samples =
          if s.samples < 2 then 1 else s.samples / 2
        else if key == SDLK_w
        then s with cam = move_camera s.cam (mkvec3 0 0 1)
        else if key == SDLK_a
        then s with cam = move_camera s.cam (mkvec3 (-1) 0 0)
        else if key == SDLK_s
        then s with cam = move_camera s.cam (mkvec3 0 0 (-1))
        else if key == SDLK_d
        then s with cam = move_camera s.cam (mkvec3 1 0 0)
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
