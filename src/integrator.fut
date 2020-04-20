import "shapes"
import "material"
import "state"
import "direct"


let color (lr: lightray)
          (scene: accel_scene)
          (ambience: spectrum)
          (rng: rnge)
        : f32 =
  let tmax = f32.highest
  let ambience = spectrum_lookup lr.wavelen ambience
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
        case #nothing -> finish (radiance + ambience)

let one_sample_pixel (scene: accel_scene)
                     (cam: camera)
                     (ambience: spectrum)
                     (w: f32, h: f32)
                     (j: u32, i: u32)
                     (rng: rnge)
                   : vec3 =
  let (rng, wl, wl_radiance_to_rgb) =
    sample_camera_wavelength cam rng
  let r = sample_camera_ray cam
                            (mkvec2 w h)
                            (mkvec2 (f32.u32 j) (h - f32.u32 i - 1.0))
                            rng
  -- TODO: When lidar, create very thin spotlight based on the
  --       direction of the ray.
  let lr = { r, wavelen = wl }
  let scene = scene with lights = scene.lights ++ gen_transmitter cam r
  in vec3.scale (color lr scene ambience rng) wl_radiance_to_rgb

let sample_pixel (s: state)
                 (w: u32, h: u32)
                 (j: u32, i: u32)
                 (rng: rnge)
               : vec3 =
  let rngs = rnge.split_rng (i32.u32 s.samples) rng
  let sample' rng =
    (vec3./) (one_sample_pixel s.scene
                               s.cam
                               s.ambience
                               (f32.u32 w, f32.u32 h)
                               (j, i)
                               rng)
             (mkvec3_repeat (f32.u32 s.samples))
  in reduce_comm (vec3.+)
                 (mkvec3_repeat 0)
                 (map sample' rngs)

let sample_pixels (s: state): (rnge, [][]vec3) =
  let (w, h) = s.dimensions
  let (w, h) = ( (w + s.subsampling - 1) / s.subsampling
               , (h + s.subsampling - 1) / s.subsampling)
  let rngs = rnge.split_rng (i32.u32 (w * h)) s.rng
  let img = tabulate_2d (i32.u32 h) (i32.u32 w)
                        (\i j -> let ix = i * i32.u32 w + j
                                 let rng = rngs[ix]
                                 in sample_pixel s
                                                 (w, h)
                                                 (u32.i32 j, u32.i32 i)
                                                 rng)
  in (advance_rng s.rng, img)

let sample_pixels_accum [m] [n] (s: state): (rnge, [m][n]vec3) =
  let (rng, img_new) = sample_pixels s
  let nf = f32.u32 s.n_frames
  let merge acc c = vec3.scale ((nf - 1) / nf) acc
                    vec3.+ vec3.scale (1 / nf) c
  in (rng, map2 (map2 merge) s.img (img_new :> [m][n]vec3))
