import "../lib/github.com/athas/matte/colour"
import "material"
import "bvh"
import "camera"
import "state"
import "integrator"
import "sdl"

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
     , mode = false
     , cam = { pitch = 0.0
             , yaw = 0.0
             , origin = mkvec3 0 0.8 1.8
             , aperture = 0.0
             , focal_dist = 1.5
             , transmitter = #none }
     , lidar_mode = false
     , scene = accelerate_scene raw_scene }

entry resize (h: u32) (w: u32) (s: state): state =
    s with dimensions = (w, h) with mode = false

entry step (dt: f32) (s: state): state =
  let time = s.time + dt
  let ((rng, img), n_frames) =
    if s.mode
    then (sample_accum s, s.n_frames + 1)
    else (sample_all s, 1)
  in s with img = img
       with rng = rng
       with time = time
       with n_frames = n_frames

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
       then s with lidar_mode = !s.lidar_mode
              with mode = false
       else if key == SDLK_8
       then s with cam = (s.cam with transmitter = #flash { radius = 0.1, emission = uniform_spectrum 400 })
       else if key == SDLK_9
       then s with cam = (s.cam with transmitter = #scanning { radius = 0.1, theta = from_deg 60, emission = uniform_spectrum 100000 })
       else if key == SDLK_0
       then s with cam = (s.cam with transmitter = #none)
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
