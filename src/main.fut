import "../lib/github.com/athas/matte/colour"
import "material"
import "bvh"
import "camera"
import "state"
import "integrator"
import "sdl"

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
let camera_sensor: sensor =
  [ ({ mu = 455, sigma = 22 }, mkvec3 0 0 1)
  , ({ mu = 535, sigma = 32 }, mkvec3 0 1 0)
  , ({ mu = 610, sigma = 26 }, mkvec3 1 0 0) ]
let lidar_sensor: sensor =
  [ ({ mu = 1550, sigma = 10 }, mkvec3 1 0 0) ]

type text_content = (u32, u32, u32, f32, f32, u32)

entry grab_mouse: bool = false

entry init (_seed: u32)
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
     , ambience = bright_blue_sky
     , mode = false
     , render_mode = #render_color
     , cam = { pitch = 0.0
             , yaw = 0.0
             , origin = mkvec3 0 0.8 1.8
             , aperture = 0.0
             , focal_dist = 1.5
             , offset_radius = 1.0
             , sensor = copy camera_sensor
             , transmitter = #none }
     , scene = accelerate_scene raw_scene }

entry resize (h: u32) (w: u32) (s: state): state =
    s with dimensions = (w, h) with mode = false

entry step (dt: f32) (s: state): state =
  let time = s.time + dt
  let ((rng, img), n_frames) =
    if s.mode
    then (sample_pixels_accum s, s.n_frames + 1)
    else let channels = sensor_channel_visualizations s.cam.sensor
         let (rng, ps) = sample_pixels s
         let img = visualize_pixels s.render_mode channels ps
         in ((rng, img), 1)
  in s with img = img
       with rng = rng
       with time = time
       with n_frames = n_frames

let lidar_mode (s: state): state =
  s with cam = (s.cam with sensor = copy lidar_sensor
                      with offset_radius = 0.01)
    with mode = false
    with render_mode = #render_distance

let camera_mode (s: state): state =
  s with cam = (s.cam with sensor = copy camera_sensor
                      with offset_radius = 1)
    with mode = false
    with render_mode = #render_color

entry key (e: i32) (key: i32) (s: state): state =
  let keydown = e == 0 in
  if keydown
  then if key == SDLK_e
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
       else if key == SDLK_t
       then if length s.cam.sensor == 3
            then lidar_mode s
            else camera_mode s
       else if key == SDLK_8
       then s with cam = (s.cam with transmitter = #flash { radius = 0.05, emission = map_intensities (* 1000) (blackbody_normalized 5500) })
       else if key == SDLK_9
       then s with cam = (s.cam with transmitter = #scanning { radius = 0.01, theta = from_deg 3, emission = uniform_spectrum 1500 })
       else if key == SDLK_0
       then s with cam = (s.cam with transmitter = #none)
       else if key == SDLK_p
       then s with ambience = if s.ambience.b0.1 == 0
                              then bright_blue_sky
                              else uniform_spectrum 0
       else s
  else s

entry render (s: state): [][]argb.colour =
  let vcol_to_argb (c: vec3): argb.colour =
    argb.from_rgba c.x c.y c.z 1f32
  let (full_w, full_h) = let (w, h) = s.dimensions in (i32.u32 w, i32.u32 h)
  let subsampling = i32.u32 s.subsampling
  let upscaled =
    tabulate_2d full_h full_w
                (\i j -> unsafe s.img[ i / subsampling
                                     , j / subsampling ])
  in map (map vcol_to_argb) upscaled

entry text_format: []u8 =
  "FPS: %d\nSAMPLES: %d\nACCUM FRAMES: %d\nAPERTURE: %.2f\nFOCAL DIST: %.2f\nSUBSAMPLING: %d"

entry text_content (fps: f32) (s: state): text_content =
  (u32.f32 fps, s.samples, s.n_frames , s.cam.aperture, s.cam.focal_dist, s.subsampling )

entry text_colour (_: state): argb.colour = argb.yellow
