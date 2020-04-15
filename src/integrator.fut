import "../lib/github.com/diku-dk/statistics/statistics"
import "shapes"
import "material"
import "state"
import "direct"

module stat = mk_statistics f32


let color (lr: lightray) (scene: accel_scene) (rng: rnge)
        : f32 =
  let tmax = f32.highest
  let full_sky =
    { b0 = (red_wavelen, 0.6)
    , b1 = (green_wavelen, 0.7)
    , b2 = (blue_wavelen, 0.8)
    , b3 = (-1, 0)
    , b4 = (-1, 0)
    , b5 = (-1, 0) }
  let sky = spectrum_lookup lr.wavelen full_sky
  -- Choke throughput to end the loop, returning the radiance
  let finish radiance =
    -- Arbitrary values
    let (a_lray, a_bounced, a_rng) = (lr, true, rng)
    in (radiance, false, a_lray, a_bounced, a_rng)
  let continue radiance lray rng =
    (radiance, true, lray, true, rng)
  in (.0) <|
     loop (radiance, should_continue, lr, has_bounced, rng) =
          (0, true, lr, false, rng)
     while should_continue
     do match closest_interaction tmax lr scene.mats scene.objs
        case #just i ->
          let rng = advance_rng rng
          let wo = vec3_neg lr.r.dir
          let (rng, direct_radiance) = direct_radiance rng wo i scene
          let radiance = radiance
                         + direct_radiance
                         + if !has_bounced then spectrum_lookup lr.wavelen i.mat.emission
                                           else 0
          let (rng, { wi, bsdf, pdf }) = sample_dir wo i rng
          let pdf = match pdf
                    case #impossible -> 0
                    case #delta -> 1
                    case #nonzero x -> x
          let cosFalloff = f32.abs (vec3.dot i.h.normal wi)
          -- Russian roulette termination. Instead of absolutely cutting off
          -- the "recursion" after N bounces, keep going with some probability
          -- and weight the samples appropriately. When we do it like this,
          -- the final result is an unbiased estimate of the sum. See PBR Book
          -- 14.5.1, 13.7.
          let p_terminate = 1 - bsdf * cosFalloff / pdf
          let (rng, terminate) = map_snd (< p_terminate) (random_unit_exclusive rng)
          in if pdf == 0 || terminate
             then finish radiance
             else continue radiance
                           (lr with r = mkray_adjust_acne i.h wi)
                           rng
        case #nothing -> finish (radiance + sky)

let sample (scene: accel_scene)
           (cam: camera)
           (lidar_mode: bool)
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
  let lidar_sensor =
    [ ({ mu = 1550, sigma = 10 }, mkvec3 1 0 0) ]
  let sensor = if lidar_mode then lidar_sensor else camera_sensor
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
  let lr = { r, wavelen }
  in vec3.scale (color lr scene rng) wavelen_radiance_to_rgb

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
                        s.lidar_mode
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
