import "lys/lys"
import "lib/github.com/diku-dk/statistics/statistics"

import "material"
import "bvh"
import "camera"

module stat = mk_statistics f32

module xbvh = lbvh

let mkray (o: vec3) (d: vec3): ray =
  { origin = o, dir = vec3.normalise(d) }

type arealight = { geom: geom, emission: spectrum }

type light = #pointlight { pos: vec3, emission: spectrum }
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
  in vec3.dot w h.normal <= 0
     || (let distance = vec3.norm v
         let r = mkray_adjust_acne h w
         in xbvh.any_hit (distance - eps) r objs)

type light_sample = { pos: vec3, wi: vec3, in_radiance: f32, pdf: f32 }

let trianglelight_incident_radiance (hitp: vec3) (lightp: vec3) (t: triangle) (wavelen: f32) (emission: spectrum): f32 =
  let (wi, distance_sq) =
    let v = lightp vec3.- hitp
    in (vec3.normalise v, vec3.quadrance v)
  let (e1, e2) = (t.b vec3.- t.a, t.c vec3.- t.a)
  let lnormal = vec3.normalise (vec3.cross e1 e2)
  let cos_theta_l = vec3.dot (vec3_neg wi) lnormal
  in f32.max 0
             (spectrum_lookup wavelen emission * cos_theta_l / distance_sq)

let arealight_incident_radiance (hitp: vec3) (lightp: vec3) (wavelen: f32) (light: arealight): f32 =
  match light.geom
  case #triangle t -> trianglelight_incident_radiance hitp lightp t wavelen light.emission
  case #sphere _ -> 0

let triangle_area (t: triangle): f32 =
  let e1 = t.b vec3.- t.a
  let e2 = t.c vec3.- t.a
  in vec3.norm (vec3.cross e1 e2) / 2

let arealight_pdf (l: arealight): f32 =
  match l.geom
  case #sphere _ -> 0
  case #triangle t -> 1 / triangle_area t

let sample_pointlight (h: hit) (pos: vec3) (wavelen: f32) (emission: spectrum)
                    : light_sample =
  let (wi, distance_sq) =
    let v = pos vec3.- h.pos
    in (vec3.normalise v, vec3.quadrance v)
  let in_radiance = spectrum_lookup wavelen emission / distance_sq
  in { pos, wi, in_radiance, pdf = 1 }

let sample_arealight (rng: rnge) (h: hit) (wavelen: f32) (l: arealight)
                   : (rnge, light_sample) =
    match l.geom
    case #triangle t ->
      let e1 = t.b vec3.- t.a
      let e2 = t.c vec3.- t.a
      let area = vec3.norm (vec3.cross e1 e2) / 2
      let (_rng, (u, v)) = random_in_triangle rng
      let p = vec3.(t.a + scale u e1 + scale v e2)
      let wi = vec3.normalise (p vec3.- h.pos)
      let in_radiance = trianglelight_incident_radiance h.pos p t wavelen l.emission
      in (rng, { pos = p, wi, in_radiance, pdf = 1 / area })
    -- TODO
    case #sphere _ ->
      (rng, { pos = mkvec3 0 0 0
            , wi = mkvec3 0 0 0
            , in_radiance = 0
            , pdf = 0 })

let sample_light (rng: rnge) (h: hit) (wavelen: f32) (l: light) (objs: xbvh.bvh)
               : (rnge, light_sample) =
  let (rng, light_sample) =
    match l
    case #pointlight { pos, emission } ->
      (rng, sample_pointlight h pos wavelen emission)
    case #arealight a ->
      sample_arealight rng h wavelen a
  in (rng, if occluded h light_sample.pos objs
           then light_sample with in_radiance = 0
           else light_sample)

-- The Balance heuristic of Multiple Importance Sampling
let balance_heuristic (nf: u32, pdf_f: f32) (ng: u32, pdf_g: f32): f32 =
  let (nf, ng) = (f32.u32 nf, f32.u32 ng)
  in nf * pdf_f / (nf * pdf_f + ng * pdf_g)

-- TODO: Oh jeez we gotta make this more readable 100%
--
-- Estimate direct light contribution using Multiple Importance Sampling.
let estimate_direct (rng: rnge)
                    (wo: vec3)
                    (h: hit)
                    (wavelen: f32)
                    (l: light)
                    (objs: xbvh.bvh)
                  : (rnge, f32) =
  -- Sample light with MIS
  let (rng, light_radiance) =
    let (rng, { pos = _, wi, in_radiance, pdf }) =
      sample_light rng h wavelen l objs
    in if pdf == 0 || in_radiance == 0
       then (rng, 0)
       else let f = bsdf_f wo wi h wavelen
                    * f32.abs (vec3.dot wi h.normal)
            let scattering_pdf = bsdf_pdf wo wi h wavelen
            let weight = balance_heuristic (1, pdf) (1, scattering_pdf)
            in (rng, f * weight * in_radiance / pdf)
  -- Sample BSDF with MIS
  let (rng, bsdf_radiance) =
    match l
    case #pointlight _ -> (rng, 0)
    case #arealight l ->
      let (rng, { wi, bsdf, pdf }) = sample_dir wo h wavelen rng
      in ( rng
         , let r = mkray_adjust_acne h wi
           in match hit_geom f32.highest r l.geom
              case #nothing -> 0
              case #just lh ->
                if occluded h lh.pos objs
                then 0
                else let in_radiance =
                       arealight_incident_radiance h.pos lh.pos wavelen l
                     let f = bsdf * f32.abs (vec3.dot wi h.normal)
                     in match pdf
                        case #impossible -> 0
                        case #delta -> f * in_radiance
                        case #nonzero pdf ->
                          let light_pdf = arealight_pdf l
                          let weight =
                            balance_heuristic (1, pdf) (1, light_pdf)
                          in f * in_radiance * weight / pdf)
  in (rng, light_radiance + bsdf_radiance)

-- Compute the direct radiance by stochastically sampling one light,
-- taking the reflection direction into account with Multiple
-- Importance Sampling to eliminate fireflies and get caustics.
--
-- Basically equivalent to `UniformSampleOneLight` of PBR Book 14.3.
let direct_radiance (rng: rnge)
                    (wo: vec3)
                    (h: hit)
                    (wavelen: f32)
                    (scene: accel_scene)
                  : (rnge, f32) =
  if null scene.lights
  then (rng, 0)
  else let (rng, l) = random_select rng scene.lights
       let (rng, radiance) =
         estimate_direct rng wo h wavelen l scene.objs
       let light_pdf = 1 / f32.i32 (length scene.lights)
       in (rng, radiance / light_pdf)

let color (r: ray) (wavelen: f32) (scene: accel_scene) (rng: rnge)
        : f32 =
  let tmax = f32.highest
  let full_sky =
    { b0 = (red_wavelen, 0.6)
    , b1 = (green_wavelen, 0.7)
    , b2 = (blue_wavelen, 0.8)
    , b3 = (-1, 0)
    , b4 = (-1, 0)
    , b5 = (-1, 0) }
  let sky = spectrum_lookup wavelen full_sky
  -- Choke throughput to end the loop, returning the radiance
  let finish radiance =
    -- Arbitrary values
    let (a_ray, a_bounced, a_rng) = (r, true, rng)
    in (radiance, false, a_ray, a_bounced, a_rng)
  let continue radiance ray rng =
    (radiance, true, ray, true, rng)
  in (.0) <|
     loop (radiance, should_continue, r, has_bounced, rng) =
          (0, true, r, false, rng)
     while should_continue
     do match xbvh.closest_hit tmax r scene.mats scene.objs
        case #just h ->
          let rng = advance_rng rng
          let wo = vec3_neg r.dir
          let (rng, direct_radiance) = direct_radiance rng wo h wavelen scene
          let radiance = radiance
                         + direct_radiance
                         + if !has_bounced then spectrum_lookup wavelen h.mat.emission
                                           else 0
          let (rng, { wi, bsdf, pdf }) = sample_dir wo h wavelen rng
          let pdf = match pdf
                    case #impossible -> 0
                    case #delta -> 1
                    case #nonzero x -> x
          let cosFalloff = f32.abs (vec3.dot h.normal wi)
          -- Russian roulette termination. Instead of absolutely cutting off
          -- the "recursion" after N bounces, keep going with some probability
          -- and weight the samples appropriately. When we do it like this,
          -- the final result is an unbiased estimate of the sum. See PBR Book
          -- 14.5.1, 13.7.
          let p_terminate = 1 - bsdf * cosFalloff / pdf
          let (rng, terminate) = map_snd (< p_terminate) (random_unit_exclusive rng)
          in if pdf == 0 || terminate
             then finish radiance
             else let r = mkray_adjust_acne h wi
                  in continue radiance r rng
        case #nothing -> finish (radiance + sky)

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
  -- TODO: When lidar, don't apply random offset, and don't "stretch"
  --       the point to cover the whole pixel, so to speak.
  let xy = (ji vec2.+ offset) vec2./ wh
  -- Spectral sensitivities of the camera sensor, approximated with normal distributions.
  --
  -- Possibly helpful data:
  --   Jiang's paper and database:
  --     http://www.gujinwei.org/research/camspec/camspec.pdf
  --     http://www.gujinwei.org/research/camspec/db.html
  --   Some database (with plotted JPGs!) from University of Tokyo:
  --     https://nae-lab.org/~rei/research/cs/zhao/database.html
  --
  -- For prototyping purposes we've (arbitrarily) chosen to model the
  -- Canon 400D, measured in the above UoT
  -- database. https://nae-lab.org/~rei/research/cs/zhao/files/canon_400d.jpg
  let camera_sensor =
    [ ({ mu = 455, sigma = 22 }, mkvec3 0 0 1)
    , ({ mu = 535, sigma = 32 }, mkvec3 0 1 0)
    , ({ mu = 610, sigma = 26 }, mkvec3 1 0 0) ]
  -- let lidar_sensor =
  --   [ ({ mu = 1550, sigma = 10 }, mkvec3 1 0 0) ]
  let sensor = camera_sensor
  -- TODO: Should probably not just be 1/n. The ration of area of
  --       distribution / area of all distributions?
  let (rng, (wavelen_distr, wavelen_radiance_to_rgb)) =
    random_select rng sensor
  let wavelen_radiance_to_rgb =
    vec3.scale (f32.i32 (length sensor)) wavelen_radiance_to_rgb
  -- Sample an `x` value (wavelength) from the normal distribution with
  -- inverse transform sampling of normal distribution
  -- = probit
  -- = quantile function of normal distribution
  -- = inverse CDF of normal distribution
  -- = statistics.sample
  let (rng, p) = random_unit_exclusive rng
  let wavelen = stat.sample (stat.mk_normal wavelen_distr) p
  let r = get_ray cam ratio xy rng
  -- TODO: When lidar, create very thin spotlight based on the
  --       direction of the ray.
  in vec3.scale (color r wavelen scene rng) wavelen_radiance_to_rgb

let nm_to_m: f32 -> f32 = (* 1e-9)
let m_to_nm: f32 -> f32 = (* 1e9)

-- Computes the emitted radiance at the given temperature T in kelvin
-- according to Planck's Law.
--
-- PBR book 12.1.1
let blackbody (T: f32): spectrum =
  let c = 299792458
  let h = 6.62606957e-34
  let kb = 1.3806488e-23
  let ls = map nm_to_m [150, blue_wavelen, green_wavelen, red_wavelen, 1000, 2000]
  let planck l =
    (2 * h * c * c)
    / ((l**5) * (f32.exp ((h * c) / (l * kb  * T)) - 1))
  in spectrum_from_arr (map (\l -> (m_to_nm l, planck l)) ls)

let blackbody_normalized (T: f32): spectrum =
  let radiance = blackbody T
  let wiens_displacement = 2.8977721e-3
  let lambda_max = m_to_nm (wiens_displacement / T)
  let max_radiance = spectrum_lookup lambda_max radiance
  in map_intensities (/ max_radiance) radiance

let get_lights ({ objs, mats }: scene): []light =
  let nonzero_spectrum s = !(null (filter (\(w, x) -> w >= 0 && x > 0)
                                          (spectrum_to_arr s)))
  let with_emission obj =
    { geom = obj.geom
    , emission = (unsafe mats[i32.u32 obj.mat_ix]).emission }
  in map (\l -> #arealight l)
     <| filter (nonzero_spectrum <-< (.emission))
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

let parse_mat (m: [28]f32): material =
  { color = { b0 = (m[0] , m[1])
            , b1 = (m[2] , m[3])
            , b2 = (m[4] , m[5])
            , b3 = (m[6] , m[7])
            , b4 = (m[8] , m[9])
            , b5 = (m[10] , m[11]) }
  , roughness = m[12]
  , metalness = m[13]
  , ref_ix = m[14]
  , opacity = m[15]
  , emission = { b0 = (m[16] , m[17])
               , b1 = (m[18] , m[19])
               , b2 = (m[20] , m[21])
               , b3 = (m[22] , m[23])
               , b4 = (m[24] , m[25])
               , b5 = (m[26] , m[27]) } }

let parse_mats (mats: [][28]f32): []material =
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
           (mat_data: [][28]f32)
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
