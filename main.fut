import "lys/lys"
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
  | #emitter { emission: vec3 }

type hit = { t: f32, pos: vec3, normal: vec3, mat: material }

type triangle = { a: vec3, b: vec3, c: vec3, mat: material }
type sphere = { center: vec3, radius: f32, mat: material }
type geom = #sphere sphere | #triangle triangle
type~ group = []geom

type bounds = { tmin: f32, tmax: f32 }

module rnge = minstd_rand
type rnge = rnge.rng

type camera = { pitch: f32, yaw: f32, origin: vec3 }

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
  case #emitter { emission } ->
    { transmit = mkvec3 1000 0 1000, wo = mkvec3 0 0 0 }

let in_bounds (bn: bounds) (t: f32): bool = t < bn.tmax && t > bn.tmin

let hit_triangle (bn: bounds) (ra: ray) (tr: triangle): maybe hit =
  -- Algorithm from RTR 22.8, variant 22.16, based on 22.8.2
  let eps = 0.00001
  let e1 = tr.b vec3.- tr.a
  let e2 = tr.c vec3.- tr.a
  let n = vec3.cross e1 e2
  let a = -(vec3.dot n ra.dir)
  in if a > -eps && a < eps then #nothing else
  let s = ra.origin vec3.- tr.a
  let m = vec3.cross s ra.dir
  let { x = t, y = u, z = v } =
    vec3.scale (1 / a)
               (mkvec3 (vec3.dot n s)
                       (vec3.dot m e2)
                       (-(vec3.dot m e1)))
  in if u < 0 || v < 0 || u + v > 1 || !(in_bounds bn t)
     then #nothing
     else let pos = point_at_param ra t
          let normal = vec3.normalise n
          in #just { t, pos, normal, mat = tr.mat }

let hit_sphere (bn: bounds) (r: ray) (s: sphere): maybe hit =
  let oc = r.origin vec3.- s.center
  let a = 1 -- vec3.dot r.dir r.dir
  let b = 2 * vec3.dot oc r.dir
  let c = vec3.dot oc oc - s.radius * s.radius
  let discriminant = b * b - 4 * a * c
  let root0 = (-b - f32.sqrt discriminant) / (2 * a)
  let root1 = (-b + f32.sqrt discriminant) / (2 * a)
  let handle_root t =
    let pos = point_at_param r t
    let normal = vec3.scale (1 / s.radius) (pos vec3.- s.center)
    in #just { t, pos, normal, mat = s.mat }
  in if discriminant > 0
     then if in_bounds bn root0 then handle_root root0
          else if in_bounds bn root1 then handle_root root1
          else #nothing
     else #nothing

let hit_geom (bn: bounds) (r: ray) (g: geom): maybe hit =
  match g
  case #sphere s -> hit_sphere bn r s
  case #triangle t -> hit_triangle bn r t

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
       case #just {mat = #emitter {emission}, normal, pos, t} ->
         (throughput vec3.* emission, r, rng, 0)
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

let cam_dir (cam: camera): vec3 =
  vec3.normalise
    (mkvec3 (f32.sin cam.yaw) (f32.sin cam.pitch) (-(f32.cos cam.yaw)))

let cam_right (cam: camera): vec3 =
  vec3.normalise (vec3.cross (cam_dir cam) world_up)

let cam_up (cam: camera): vec3 =
  vec3.normalise (vec3.cross (cam_right cam) (cam_dir cam))

let get_ray (cam: camera) (ratio: f32) (coord: vec2): ray =
  let field_of_view = 80.0
  let f = (to_radians field_of_view) / 2.0
  let a = vec3.scale (f32.cos f) (cam_dir cam)
  let b = vec3.scale (f32.sin f) (cam_up cam)
  let c = vec3.scale (f32.sin f * ratio) (cam_right cam)
  let screen_bot_left = a vec3.- c vec3.- b
  let screen_x_dir = vec3.scale 2 (a vec3.- b vec3.- screen_bot_left)
  let screen_y_dir = vec3.scale 2 (a vec3.- c vec3.- screen_bot_left)
  in mkray cam.origin
           (screen_bot_left
            vec3.+ vec3.scale coord.x screen_x_dir
            vec3.+ vec3.scale coord.y screen_y_dir)

let sample (w: u32, h: u32)
           (j: u32, i: u32)
           (offset: vec2)
           (rng: rnge)
           (cam: camera)
           (world: group)
         : vec3 =
  let wh = mkvec2 (f32.u32 w) (f32.u32 h)
  let ji = mkvec2 (f32.u32 j) (f32.u32 h - f32.u32 i - 1.0)
  let xy = (ji vec2.+ offset) vec2./ wh
  let ratio = f32.u32 w / f32.u32 h
  let r = get_ray cam ratio xy
  in color r world rng

let sample_all (n: u32)
               (w: u32, h: u32)
               (rng: rnge)
               (cam: camera)
               (world: group)
             : (rnge, [][]vec3) =
  let rngs = rnge.split_rng (i32.u32 n) rng
  let rngss = map (rnge.split_rng (i32.u32 (w * h))) rngs
  let sample' i j rngs =
    let ix = i * i32.u32 w + j
    let rng = rngs[ix]
    let (rng, offset_x) = dist.rand (0,1) rng
    let (rng, offset_y) = dist.rand (0,1) rng
    let offset = mkvec2 offset_x offset_y
    in (vec3./) (sample (w, h) (u32.i32 j, u32.i32 i)
                        offset rng cam world)
                (mkvec3_repeat (f32.u32 n))
  let img = tabulate_2d (i32.u32 h) (i32.u32 w) <| \i j ->
              reduce_comm (vec3.+)
                          (mkvec3_repeat 0)
                          (map (sample' i j) rngss)
  in (advance_rng rng, img)

let sample_accum (n_frames: u32)
                 (dims: (u32, u32))
                 (rng: rnge)
                 (cam: camera)
                 (img_acc: [][]vec3)
                 (world: group)
               : (rnge, [][]vec3) =
  let (rng, img_new) = sample_all 1 dims rng cam world
  let nf = f32.u32 n_frames
  let merge acc c = vec3.scale ((nf - 1) / nf) acc
                    vec3.+ vec3.scale (1 / nf) c
  in (rng, map2 (map2 merge) img_acc img_new)

let move_camera (cam: camera) (m: vec3): camera =
  let cam_forward = vec3.normalise (cam_dir cam with y = 0)
  in cam with origin = cam.origin
                       vec3.+ vec3.scale (0.1*m.z) cam_forward
                       vec3.+ vec3.scale (0.1*m.x) (cam_right cam)
                       vec3.+ vec3.scale (0.1*m.y) world_up

let turn_camera (cam: camera) (pitch: f32) (yaw: f32): camera =
  cam with pitch = (f32.min (0.5*f32.pi)
                            (f32.max (-0.5*f32.pi)
                                     (cam.pitch + pitch)))
      with yaw = (cam.yaw + yaw) % (2*f32.pi)

let vec3_from_array (xs: [3]f32): vec3 =
  { x = xs[0], y = xs[1], z = xs[2] }

let parse_triangles (xs: []f32): []geom =
  let mat = #metal { albedo = mkvec3 1 0.8 0, fuzz = 0.2 }
  let f ys = let ys' = (map vec3_from_array ys)
             in #triangle { a = ys'[0], b = ys'[1], c = ys'[2], mat }
  in map f (unflatten_3d (length xs / 9) 3 3 xs)

type text_content = (u32, u32, u32)
module lys: lys with text_content = text_content = {
  type~ state = { time: f32
                , dimensions: (u32, u32)
                , rng: minstd_rand.rng
                , img: [][]vec3
                , samples: u32
                , n_frames: u32
                , mode: bool
                , cam: camera
                , world: group }
  let grab_mouse = false

  let init (seed: u32) (h: u32) (w: u32) (data: []f32): state =
    { time = 0
    , dimensions = (w, h)
    , rng = minstd_rand.rng_from_seed [123]
    , img = tabulate_2d (i32.u32 h) (i32.u32 w) (\_ _ -> mkvec3 0 0 0)
    , samples = 1
    , n_frames = 1
    , mode = false
    , cam = { pitch = 0.0, yaw = 0.0
              , origin = mkvec3 0 0.1 0.5 }
    , world =
        [ #sphere
          { center = mkvec3 1.6 0 (-0.8), radius = 0.5
    	    , mat = #lambertian { albedo = mkvec3 1 1 1 } }
        , #sphere
          { center = mkvec3 0 2 (-1), radius = 0.3
    	    , mat = #emitter { emission = mkvec3 10 10 10 } }
        , #sphere
          { center = mkvec3 0 0 (-1.5), radius = 0.5
    	    , mat = #metal { albedo = mkvec3 1 0.6 0, fuzz = 0.6 } }
        , #sphere
          { center = mkvec3 0.4 0.2 (-0.6), radius = 0.5
    	    , mat = #dielectric { ref_ix = 1.6 } }
        , #sphere
          { center = mkvec3 0 (-400.4) (-1), radius = 400
      	  , mat = #lambertian { albedo = mkvec3 0.2 0.8 0.3 } }
        ] ++ parse_triangles data }

  let resize (h: u32) (w: u32) (s: state) =
    s with dimensions = (w, h) with mode = false

  let event (e: event) (s: state) =
    match e
      case #step dt ->
        let time = s.time + dt
        let ((rng, img), n_frames) =
          if s.mode
          then (sample_accum s.n_frames s.dimensions s.rng s.cam s.img s.world
               ,s.n_frames + 1)
          else (sample_all s.samples s.dimensions s.rng s.cam s.world
               ,1)
        in s with img = img with rng = rng with time = time
             with n_frames = n_frames
      case #keydown {key} ->
        if key == SDLK_e
        then s with samples = s.samples * 2
        else if key == SDLK_q
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
        else if key == SDLK_UP
        then s with cam = turn_camera s.cam (-0.1) 0.0
        else if key == SDLK_DOWN
        then s with cam = turn_camera s.cam 0.1 0.0
        else if key == SDLK_RIGHT
        then s with cam = turn_camera s.cam 0.0 0.1
        else if key == SDLK_LEFT
        then s with cam = turn_camera s.cam 0.0 (-0.1)
        else if key == SDLK_x
        then s with cam = move_camera s.cam (mkvec3 0 1 0)
        else if key == SDLK_z
        then s with cam = move_camera s.cam (mkvec3 0 (-1) 0)
        else if key == SDLK_SPACE
        then s with mode = !s.mode
        else s
      case _ -> s

  let render (s: state) = map (map vcol_to_argb) s.img

  let text_format () =
    "FPS: %d\nGOTTA GO FAST\nSAMPLES: %d\nN ACCUM FRAMES: %d"

  type text_content = text_content

  let text_content (render_duration: f32) (s: state): text_content =
      (u32.f32 render_duration, s.samples, s.n_frames)

  let text_colour = const argb.yellow
}
