import "../lib/github.com/diku-dk/sorts/insertion_sort"
import "shapes"
import "material"
import "state"
import "direct"

type pixel_sample =
  { distance: f32
  , channel: i32
  , intensity: f32 }

-- Enforcing a ceiling on the path length makes our integrator more
-- approximative and not physically correct, unlike only using BSDF
-- russian roulette to terminate, which is statistical and expected
-- value that is correct. However, a max length improves performance,
-- is easier to work with when using arrays and Futhark, and still
-- looks close to accurate in most cases.
let path_len: i32 = 16

type path = [path_len]{ distance: f32, radiance: f32 }

let path_trace (lr: lightray)
               (scene: accel_scene)
               (ambience: spectrum)
               (rng: rnge)
             : path =
  let tmax = f32.highest
  let ambience = spectrum_lookup lr.wavelen ambience
  -- Choke throughput to end the loop, returning the radiance
  let finish (path: *path) =
    -- Arbitrary values
    let (a_i, a_dist, a_lray, a_rng) = (path_len, 0, lr, rng)
    in (path, a_i, a_dist, false, a_lray, a_rng)
  let continue (path: *path) i distance lray rng =
    (path, i, distance, true, lray, rng)
  let dark_path = replicate path_len { distance = f32.inf, radiance = 0 }
  in (.0) <|
     loop (path, i, distance, should_continue, lr, rng) =
          (dark_path, 0, 0, true, lr, rng)
     while should_continue && i < path_len
     do match closest_interaction tmax lr scene.mats scene.objs
        case #just inter ->
          let rng = advance_rng rng
          let wo = vec3_neg lr.r.dir
          let (rng, direct_radiance) = direct_radiance rng wo inter scene
          let radiance = direct_radiance
                       + if i == 0 then spectrum_lookup lr.wavelen inter.mat.emission
                                   else 0
          let distance = distance + inter.h.t
          let path = path with [i] = { distance, radiance }
          let (rng, { wi, bsdf, pdf }) = sample_dir wo inter rng
          let pdf = match pdf
                    case #impossible -> 0
                    case #delta -> 1
                    case #nonzero x -> x
          let cosFalloff = f32.abs (vec3.dot inter.h.normal wi)
          -- Russian roulette termination. Instead of absolutely cutting off
          -- the "recursion" after N bounces, keep going with some probability
          -- and weight the samples appropriately. When we do it like this,
          -- the final result is an unbiased estimate of the sum. See PBR Book
          -- 14.5.1, 13.7.
          let p_terminate = 1 - bsdf * cosFalloff / pdf
          let (rng, terminate) = map_snd (< p_terminate) (random_unit_exclusive rng)
          in if pdf == 0 || terminate
             then finish path
             else continue path
                           (i + 1)
                           distance
                           (lr with r = mkray_adjust_acne inter.h wi)
                           rng
        case #nothing -> finish (path with [i] = { distance = f32.inf, radiance = ambience })

let sample_pixel (scene: accel_scene)
                     (cam: camera)
                     (ambience: spectrum)
                     (w: f32, h: f32)
                     (j: u32, i: u32)
                     (rng: rnge)
                   : [path_len]pixel_sample =
  let (rng, wl, channel) =
    sample_camera_wavelength cam rng
  let r = sample_camera_ray cam
                            (mkvec2 w h)
                            (mkvec2 (f32.u32 j) (h - f32.u32 i - 1.0))
                            rng
  -- TODO: When lidar, create very thin spotlight based on the
  --       direction of the ray.
  let lr = { r, wavelen = wl }
  let scene = scene with lights = scene.lights ++ gen_transmitter cam r
  in map (\{ distance, radiance } -> { distance
                                     , intensity = radiance
                                     , channel })
         (path_trace lr scene ambience rng)

let sample_pixels (s: state): (rnge, [][][path_len]pixel_sample) =
  let (w, h) = s.dimensions
  let (w, h) = ( (w + s .subsampling - 1) / s.subsampling
               , (h + s.subsampling - 1) / s.subsampling)
  let rngs = rnge.split_rng (i32.u32 (w * h)) s.rng
  let sample' rng ji =
    sample_pixel s.scene s.cam s.ambience
                 (f32.u32 w, f32.u32 h) ji
                 rng
  let img = tabulate_2d (i32.u32 h) (i32.u32 w) <| \i j ->
    let ix = i * i32.u32 w + j
    let rng = rngs[ix]
    in sample' rng (u32.i32 j, u32.i32 i)
  in (advance_rng s.rng, img)

-- If rendering lidar data, convert to color based on distance of
-- closest sample. If rendering as visible light, average the radiance
-- values of all samples for a pixel.
let visualize_pixels [n] [m]
                     (render_mode: render_mode)
                     (channels: []vec3)
                     (pixels_samples: [n][m][]pixel_sample)
                   : [n][m]vec3 =
  -- HSV to RGB, using max value and saturation.
  let hue_to_rgb h =
    let h' = h * 6
    let x = 1 - f32.abs (h' % 2 - 1)
    in match u32.f32 h'
       case 0 -> mkvec3 1 x 0
       case 1 -> mkvec3 x 1 0
       case 2 -> mkvec3 0 1 x
       case 3 -> mkvec3 0 x 1
       case 4 -> mkvec3 x 0 1
       case _ -> mkvec3 1 0 x

  let visualize (samples: []pixel_sample): vec3 =
    match render_mode
    case #render_distance ->
      let (min_d, max_d) = (0.5, 10)
      let distance_to_hue d = 0.85 * (d - min_d) / (max_d - min_d)
      let ss = samples
            |> filter (\s -> s.intensity > 0
                             && s.distance > min_d
                             && s.distance < max_d)
            |> insertion_sort (\a b -> a.distance <= b.distance)
      in if null ss
         then mkvec3 0 0 0
         else hue_to_rgb (distance_to_hue (head ss).distance)
    case #render_color ->
      vec3.scale (f32.i32 (length channels))
      <| reduce_comm (vec3.+)
                     (mkvec3_repeat 0)
                     (map (\s -> vec3.scale s.intensity channels[s.channel])
                          samples)

  in map (map visualize) pixels_samples

let sample_frame (s: state): (rnge, [][]vec3) =
  let channels = sensor_channel_visualizations s.cam.conf.sensor
  in map_snd (visualize_pixels s.render_mode channels)
             (sample_pixels s)

let sample_frame_accum [m] [n]
                       (s: state)
                     : (rnge, [m][n]vec3) =
  let (rng, img_new) = sample_frame s
  let n_frames = f32.u32 s.n_frames
  let merge_all merge_one =
    (rng, map2 (map2 merge_one) s.img (img_new :> [m][n]vec3))
  in match s.render_mode
     case #render_distance ->
       merge_all (\acc c -> if vec3.norm acc > 0 then acc else c)
     case #render_color ->
       merge_all (\acc c -> (vec3.+) (vec3.scale ((n_frames - 1) / n_frames) acc)
                                     (vec3.scale (1 / n_frames) c))
