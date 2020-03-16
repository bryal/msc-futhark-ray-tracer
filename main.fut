import "lys/lys"

import "material"
import "bvh"
import "camera"

module xbvh = lbvh

let mkray (o: vec3) (d: vec3): ray =
  { origin = o, dir = vec3.normalise(d) }

type arealight = { geom: geom, emission: vec3 }

type light = #pointlight { pos: vec3, emission: vec3 }
           | #arealight arealight

type~ scene =
  { objs: []obj
  , mats: []material }

type~ accel_scene =
  { objs: xbvh.bvh
  , mats: []material
  , lights: []light }

type~ state = { time: f32
              , dimensions: (u32, u32)
              , subsampling: u32
              , rng: minstd_rand.rng
              , img: [][]vec3
              , samples: u32
              , n_frames: u32
              , mode: bool
              , cam: camera
              , scene: accel_scene }

let vcol_to_argb (c: vec3): argb.colour =
  argb.from_rgba c.x c.y c.z 1f32

let advance_rng (rng: rnge): rnge =
  let (rng, _) = dist.rand (0,1) rng in rng

-- Create a ray from a point in a direction, fix for surface acne
let mkray_adjust_acne (h: hit) (wi: vec3): ray =
  -- Note that we don't just walk along `wi`, because then we get
  -- strange artifacts for some materials at extreme angles.
  let eps = 0.001
  let acne_offset = vec3.scale eps (same_side wi h.normal)
  in mkray (h.pos vec3.+ acne_offset) wi

let occluded (h: hit) (lightp: vec3) (objs: xbvh.bvh)
           : bool =
  let v = lightp vec3.- h.pos
  let w = vec3.normalise v
  let eps = 0.01
  -- TODO: This is wrong for refraction out of an object, right?
  in vec3.dot w h.normal <= 0
     || (let distance = vec3.norm v
         let r = mkray_adjust_acne h w
         in xbvh.any_hit (distance - eps) r objs)

type light_sample = { pos: vec3, wi: vec3, in_radiance: vec3, pdf: f32 }

let trianglelight_incident_radiance (hitp: vec3) (lightp: vec3) (t: triangle) (emission: vec3): vec3 =
  let (wi, distance_sq) =
    let v = lightp vec3.- hitp
    in (vec3.normalise v, vec3.quadrance v)
  let (e1, e2) = (t.b vec3.- t.a, t.c vec3.- t.a)
  let lnormal = vec3.normalise (vec3.cross e1 e2)
  let cos_theta_l = vec3.dot (vec3_neg wi) lnormal
  in vmax (mkvec3 0 0 0)
          (vec3.scale (cos_theta_l / distance_sq) emission)

let arealight_incident_radiance (hitp: vec3) (lightp: vec3) (light: arealight): vec3 =
  match light.geom
  case #triangle t -> trianglelight_incident_radiance hitp lightp t light.emission
  case #sphere _ -> mkvec3 0 0 0

let light_incident_radiance (hitp: vec3) (lightp: vec3) (light: light): vec3 =
  match light
  case #pointlight _ -> mkvec3 0 0 0 -- Delta distribution. Must be handled specially
  case #arealight l -> arealight_incident_radiance hitp lightp l

let triangle_area (t: triangle): f32 =
  let e1 = t.b vec3.- t.a
  let e2 = t.c vec3.- t.a
  in vec3.norm (vec3.cross e1 e2) / 2

let arealight_pdf (l: arealight): f32 =
  match l.geom
  case #sphere _ -> 0
  case #triangle t -> 1 / triangle_area t

let light_pdf (l: light): f32 =
  match l
  case #pointlight _ -> 0
  case #arealight a -> arealight_pdf a

let sample_pointlight (h: hit) (pos: vec3) (emission: vec3)
                    : light_sample =
  let (wi, distance_sq) =
    let v = pos vec3.- h.pos
    in (vec3.normalise v, vec3.quadrance v)
  let in_radiance = vec3.scale (1 / distance_sq) emission
  in { pos, wi, in_radiance, pdf = 1 }

let sample_arealight (rng: rnge) (h: hit) (l: arealight)
                   : (rnge, light_sample) =
    match l.geom
    case #triangle t ->
      let e1 = t.b vec3.- t.a
      let e2 = t.c vec3.- t.a
      let area = vec3.norm (vec3.cross e1 e2) / 2
      let (_rng, (u, v)) = random_in_triangle rng
      let p = t.a vec3.+ vec3.scale u e1 vec3.+ vec3.scale v e2
      let wi = vec3.normalise (p vec3.- h.pos)
      let in_radiance = trianglelight_incident_radiance h.pos p t l.emission
      in (rng, { pos = p, wi, in_radiance, pdf = 1 / area })
    -- TODO
    case #sphere _ ->
      (rng, { pos = mkvec3 0 0 0
            , wi = mkvec3 0 0 0
            , in_radiance = mkvec3 0 0 0
            , pdf = 1 })

let sample_light (rng: rnge) (h: hit) (l: light) (objs: xbvh.bvh)
               : (rnge, light_sample) =
  let (rng, light_sample) =
    match l
    case #pointlight { pos, emission } ->
      (rng, sample_pointlight h pos emission)
    case #arealight a ->
      sample_arealight rng h a
  in (rng, if occluded h light_sample.pos objs
           then light_sample with in_radiance = mkvec3 0 0 0
           else light_sample)

-- The Balance heuristic of Multiple Importance Sampling
let balance_heuristic (nf: u32, pdf_f: f32) (ng: u32, pdf_g: f32): f32 =
  let (nf, ng) = (f32.u32 nf, f32.u32 ng)
  in nf * pdf_f / (nf * pdf_f + ng * pdf_g)

-- TODO: LTDE Caustics don't seem to work. Why?
let estimate_direct (rng: rnge) (wo: vec3) (h: hit) (l: light) (objs: xbvh.bvh)
                  : (rnge, vec3) =
  -- Sample light with MIS
  let (rng, light_radiance) =
    let (rng, { pos, wi, in_radiance, pdf }) = sample_light rng h l objs
    in if in_radiance == mkvec3 0 0 0
       then (rng, mkvec3 0 0 0)
       else let f = vec3.scale (f32.abs (vec3.dot wi h.normal)) (bsdf_f wo wi h)
            let scattering_pdf = bsdf_pdf wo wi h
            let weight = balance_heuristic (1, pdf) (1, scattering_pdf)
            in (rng, vec3.scale (weight / pdf) (f vec3.* in_radiance))
  -- Sample BSDF with MIS
  let (rng, bsdf_radiance) =
    match l
    case #pointlight _ -> (rng, mkvec3 0 0 0)
    case #arealight l ->
      let (rng, { wi, bsdf, pdf }) = sample_dir wo h rng
      in if bsdf == mkvec3 0 0 0 || pdf == 0
         then (rng, mkvec3 0 0 0)
         else let r = mkray_adjust_acne h wi
              in match hit_geom f32.highest r l.geom
                 case #just lh ->
                   if occluded h lh.pos objs
                   then (rng, mkvec3 0 0 0)
                   else let in_radiance = arealight_incident_radiance h.pos lh.pos l
                        let f = vec3.scale (f32.abs (vec3.dot wi h.normal)) bsdf
                        let weight = balance_heuristic (1, pdf) (1, arealight_pdf l)
                        in (rng, vec3.scale (weight / pdf) (f vec3.* in_radiance))
                 case #nothing -> (rng, mkvec3 0 0 0)
  in (rng, light_radiance vec3.+ bsdf_radiance)

-- Compute the direct radiance by stochastically sampling one light,
-- taking the reflection direction into account with Multiple
-- Importance Sampling to eliminate fireflies and get caustics.
--
-- Basically equivalent to `UniformSampleOneLight` of PBR Book 14.3.
let direct_radiance (rng: rnge) (wo: vec3) (h: hit) (scene: accel_scene)
                  : (rnge, vec3) =
  let (rng, l) = random_select rng scene.lights
  let (rng, radiance) = estimate_direct rng wo h l scene.objs
  let n_lights = f32.i32 (length scene.lights)
  in (rng, vec3.scale n_lights radiance)

let color (r: ray) (scene: accel_scene) (rng: rnge)
        : vec3 =
  let tmax = f32.highest
  let sky = mkvec3 0.3 0.2 0.4
  -- Choke throughput to end the loop, returning the radiance
  let finish radiance =
    let choked_throughput = mkvec3 0 0 0
    -- Arbitrary values
    let (a_ray, a_bounced, a_rng) = (r, true, rng)
    in (radiance, choked_throughput, a_ray, a_bounced, a_rng)
  -- Russian roulette termination. Instead of absolutely cutting off
  -- the "recursion" after N bounces, keep going with some probability
  -- and weight the samples appropriately. When we do it like this,
  -- the final result is an unbiased estimate of the sum. See PBR Book
  -- 14.5.1.
  let p_termination = 0.1
  let roulette_terminate rng = (random_unit_exclusive rng).1 < p_termination
  in (.0) <|
     loop (radiance, throughput, r, has_bounced, rng) =
          (mkvec3 0 0 0, mkvec3 1 1 1, r, false, rng)
     while vec3.norm throughput > 0.001 && !(roulette_terminate rng)
     do let throughput = vec3.scale (1 / (1 - p_termination)) throughput
        in match xbvh.closest_hit tmax r scene.mats scene.objs
           case #just h ->
             let rng = advance_rng rng
             let wo = vec3_neg r.dir
             let (rng, direct_radiance) = direct_radiance rng wo h scene
             let radiance = radiance vec3.+ throughput vec3.*
               (direct_radiance
                vec3.+ if !has_bounced
                       then h.mat.emission
                       else mkvec3 0 0 0)
             let (rng, { wi, bsdf, pdf }) = sample_dir wo h rng
             let cosFalloff = f32.abs (vec3.dot h.normal wi)
             let throughput = throughput vec3.* (vec3.scale (cosFalloff / pdf) bsdf)
             let r = mkray_adjust_acne h wi
             in if pdf == 0
                then finish radiance
                else (radiance, throughput, r, true, rng)
           case #nothing -> finish (radiance vec3.+ (throughput vec3.* sky))

let get_ray (cam: camera) (ratio: f32) (coord: vec2) (rng: rnge): ray =
  let lens_radius = cam.aperture / 2
  let field_of_view = 80.0
  let half_height = f32.tan ((to_radians field_of_view) / 2.0)
  let half_width = ratio * half_height
  let (w, u, v) =
    (vec3.scale (-1) (cam_dir cam), cam_right cam, cam_up cam)
  let focus_dist = cam.focal_dist
  let lower_left_corner =
    cam.origin
    vec3.- vec3.scale (half_width * focus_dist) u
    vec3.- vec3.scale (half_height * focus_dist) v
    vec3.- vec3.scale focus_dist w
  let horizontal = vec3.scale (2 * half_width * focus_dist) u
  let vertical = vec3.scale (2 * half_height * focus_dist) v
  let (_, d) = random_in_unit_disk rng
  let lens = vec3.scale lens_radius d
  let lens_offset = vec3.scale lens.x u vec3.+ vec3.scale lens.y v
  let origin = cam.origin vec3.+ lens_offset
  in mkray origin
           (lower_left_corner
            vec3.+ vec3.scale coord.x horizontal
            vec3.+ vec3.scale coord.y vertical
            vec3.- origin)

let sample (scene: accel_scene)
           (cam: camera)
           (w: f32, h: f32)
           (j: u32, i: u32)
           (offset: vec2)
           (rng: rnge)
         : vec3 =
  let wh = mkvec2 w h
  let ratio = w / h
  let ji = mkvec2 (f32.u32 j) (h - f32.u32 i - 1.0)
  let xy = (ji vec2.+ offset) vec2./ wh
  let r = get_ray cam ratio xy rng
  in color r scene rng

let get_lights ({ objs, mats }: scene): []light =
  let with_emission obj =
    { geom = obj.geom
    , emission = (unsafe mats[i32.u32 obj.mat_ix]).emission }
  in map (\l -> #arealight l)
     <| filter ((> 0) <-< vec3.norm <-< (.emission))
     <| map with_emission objs

let accelerate_scene (s: scene): accel_scene =
  { objs = xbvh.build s.objs, mats = s.mats, lights = get_lights s }

let sample_all (s: state): (rnge, [][]vec3) =
  let (w, h) = s.dimensions
  let (w, h) = ( (w + s.subsampling - 1) / s.subsampling
               , (h + s.subsampling - 1) / s.subsampling)
  let rngs = rnge.split_rng (i32.u32 s.samples) s.rng
  let rngss = map (rnge.split_rng (i32.u32 (w * h))) rngs
  let sample' i j rngs =
    let ix = i * i32.u32 w + j
    let rng = rngs[ix]
    let (rng, offset_x) = dist.rand (0,1) rng
    let (rng, offset_y) = dist.rand (0,1) rng
    let offset = mkvec2 offset_x offset_y
    in (vec3./) (sample s.scene
                        s.cam
                        (f32.u32 w, f32.u32 h)
                        (u32.i32 j, u32.i32 i)
                        offset rng)
                (mkvec3_repeat (f32.u32 s.samples))
  let img = tabulate_2d (i32.u32 h) (i32.u32 w) <| \i j ->
              reduce_comm (vec3.+)
                          (mkvec3_repeat 0)
                          (map (sample' i j) rngss)
  in (advance_rng s.rng, img)

let sample_accum (s: state): (rnge, [][]vec3) =
  let (rng, img_new) = sample_all s
  let nf = f32.u32 s.n_frames
  let merge acc c = vec3.scale ((nf - 1) / nf) acc
                    vec3.+ vec3.scale (1 / nf) c
  in (rng, map2 (map2 merge) s.img img_new)

let parse_triangles [t]
                    (tris: [t][3][3]f32) (tri_mats: [t]u32)
                  : [t]obj =
  let f tri (mat_ix: u32) =
    let tri' = (map vec3_from_array tri)
    in { geom = #triangle { a = tri'[0]
                          , b = tri'[1]
                          , c = tri'[2] }
       , mat_ix }
  in map2 f tris tri_mats

let parse_mat (m: [10]f32): material =
  { color = mkvec3 m[0] m[1] m[2]
  , roughness = m[3]
  , metalness = m[4]
  , ref_ix = m[5]
  , opacity = m[6]
  , emission = mkvec3 m[7] m[8] m[9] }

let parse_mats (mats: [][10]f32): []material =
  map parse_mat mats

let upscale (full_w: i32, full_h: i32)
            (subsampling: i32)
            (sub_img: [][]vec3)
          : [][]vec3 =
  tabulate_2d full_h full_w
              (\i j -> unsafe sub_img[ i / subsampling
                                     , j / subsampling ])

type text_content = (u32, u32, u32, f32, f32, u32)
module lys: lys with text_content = text_content = {
  type~ state = state

  let grab_mouse = false

  let init (_seed: u32)
           (h: u32) (w: u32)
           (tri_geoms: [][3][3]f32)
           (tri_mats: []u32)
           (mat_data: [][10]f32)
         : state =
    let raw_scene =
      { objs = parse_triangles tri_geoms tri_mats
      , mats = parse_mats mat_data }
    in { time = 0
       , dimensions = (w, h)
       , subsampling = 2
       , rng = minstd_rand.rng_from_seed [123]
       , img = tabulate_2d (i32.u32 h) (i32.u32 w) (\_ _ -> mkvec3 0 0 0)
       , samples = 1
       , n_frames = 1
       , mode = false
       , cam = { pitch = 0.0
               , yaw = 0.0
               , origin = mkvec3 0 0.8 1.8
               , aperture = 0.0
               , focal_dist = 1.5 }
       , scene = accelerate_scene raw_scene }

  let resize (h: u32) (w: u32) (s: state) =
    s with dimensions = (w, h) with mode = false

  let event (e: event) (s: state) =
    match e
      case #step dt ->
        let time = s.time + dt
        let ((rng, img), n_frames) =
          if s.mode
          then (sample_accum s, s.n_frames + 1)
          else (sample_all s, 1)
        in s with img = img with rng = rng with time = time
             with n_frames = n_frames
      case #keydown {key} ->
        if key == SDLK_e
        then s with samples = s.samples * 2
        else if key == SDLK_q
        then s with samples =
          if s.samples < 2 then 1 else s.samples / 2
        else if key == SDLK_2
        then s with subsampling = s.subsampling + 1
               with mode = false
        else if key == SDLK_1
        then s with subsampling = u32.max 1 (s.subsampling - 1)
               with mode = false
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
        else if key == SDLK_n
        then s with mode = false
        else if key == SDLK_m
        then s with mode = true
        else if key == SDLK_i
        then s with cam =
          (s.cam with aperture = f32.min 2 (s.cam.aperture + 0.08))
        else if key == SDLK_k
        then s with cam =
          (s.cam with aperture = f32.max 0 (s.cam.aperture - 0.08))
        else if key == SDLK_o
        then s with cam =
          (s.cam with focal_dist = s.cam.focal_dist * 1.14)
        else if key == SDLK_l
        then s with cam =
          (s.cam with focal_dist = f32.max 0.1 (s.cam.focal_dist / 1.14))
        else s
      case _ -> s

  let render (s: state) =
    let dims = let (w, h) = s.dimensions in (i32.u32 w, i32.u32 h)
    let sub = i32.u32 s.subsampling
    in map (map vcol_to_argb)
           (upscale dims sub s.img)

  let text_format () =
    "FPS: %d\nSAMPLES: %d\nACCUM FRAMES: %d\nAPERTURE: %.2f\nFOCAL DIST: %.2f\nSUBSAMPLING: %d"

  type text_content = text_content

  let text_content (fps: f32) (s: state): text_content =
    (u32.f32 fps, s.samples, s.n_frames , s.cam.aperture, s.cam.focal_dist, s.subsampling )

  let text_colour = const argb.yellow
}
