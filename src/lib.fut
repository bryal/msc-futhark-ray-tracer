import "../lib/github.com/athas/matte/colour"
import "material"
import "bvh"
import "camera"
import "state"
import "integrator"
import "sdl"


let lidar_conf: camera_config =
  { aperture = 0
  , focal_dist = 1
  , offset_radius = 0.01
  , field_of_view = from_deg 90
  , sensor = [ ({ mu = 1550, sigma = 10 }, mkvec3 1 0 0) ]
  , transmitter = #scanning { radius = 0.01
                            , theta = from_deg 3
                            , emission = uniform_spectrum 1500 } }

let visual_conf: camera_config =
  { aperture = 0
  , focal_dist = 1
  , offset_radius = 1
  , field_of_view = from_deg 80
  , sensor = [ ({ mu = 455, sigma = 22 }, mkvec3 0 0 1)
             , ({ mu = 535, sigma = 32 }, mkvec3 0 1 0)
             , ({ mu = 610, sigma = 26 }, mkvec3 1 0 0) ]
  , transmitter = #none }

let visual_flash_conf: camera_config = visual_conf with transmitter =
  #flash { radius = 0.05
         , emission = map_intensities (* 1000)
                                      (blackbody_normalized 5500) }

entry sample_points_n (s: state) (samples_per_pixel: u32)
                    : (state, [][][4]f32) =
  let (n, m) = s.dimensions
  let (n, m) = (i32.u32 n, i32.u32 m)
  let factor = 1 / (f32.u32 samples_per_pixel)
  let (min_d, max_d) = (0.5, 10)
  let closest (ps: [path_len]cloud_point): cloud_point =
       map (\(p: cloud_point) -> p with intensity = p.intensity * factor) ps
    |> filter (\p -> p.intensity > 0
                     && p.distance > min_d
                     && p.distance < max_d)
    |> minimum_by (.distance)
    |> maybe.unwrap_or { pos = mkvec3_repeat (-1), distance = f32.inf, intensity = 0 }
  let merge (p1: cloud_point) (p2s: [path_len]cloud_point)
          : cloud_point =
    let p2 = closest p2s
    in if p1.distance < p2.distance then p1 else p2
  let (rng, paths) = sample_points s
  let (rng, points) =
    loop (rng, points) = (rng, map (map closest) paths :> [m][n]cloud_point)
    for _i < (i32.u32 samples_per_pixel - 1)
    do let (rng, points_new) = sample_points (s with rng = rng)
                               :> (rnge, [m][n][path_len]cloud_point)
       in ( rng
          , map2 (map2 merge) points points_new )
  in ( s with rng = rng
     , map (map (\{pos={x, y, z}, distance=_, intensity} ->
                   [x, y, z, intensity]))
           points )

-- TODO: Fewer `entry`points -> faster compilation. Futhark inlines
--       everything in each `entry`.
entry sample_n_frames (s: state) (n: u32): [][][3]f32 =
  let (rng, img) = sample_frame s
  let s = s with n_frames = 1 with rng = rng with img = img
  let s = loop (s)
          while s.n_frames < n
          do let (rng, img) = sample_frame_accum s
             in (s with img = img with rng = rng with n_frames = s.n_frames + 1)
  in map (map vec3_to_arr) s.img

entry init (seed: i32)
           (h: u32) (w: u32)
           (cam_conf_id: u32)
           (tri_geoms: [][3][3]f32)
           (tri_mats: []u32)
           (mat_data: [][28]f32)
           (cam_pitch: f32)
           (cam_yaw: f32)
           (cam_origin: [3]f32)
         : state =
  let raw_scene =
    { objs = parse_triangles tri_geoms tri_mats
    , mats = parse_mats mat_data }
  let (render_mode, conf) =
    if cam_conf_id == 0      then (#render_color, visual_conf)
    else if cam_conf_id == 1 then (#render_color, visual_flash_conf)
    else                          (#render_distance, lidar_conf)
  in { dimensions = (w, h)
     , subsampling = 1
     , rng = minstd_rand.rng_from_seed [seed]
     , img = tabulate_2d (i32.u32 h) (i32.u32 w) (\_ _ -> mkvec3 0 0 0)
     , n_frames = 0
     , ambience = no_sky
     , mode = false
     , render_mode
     , cam_conf_id
     , cam = { pitch = cam_pitch
             , yaw = cam_yaw
             , origin = vec3_from_array cam_origin
             , conf }
     , scene = accelerate_scene raw_scene }

entry resize (h: u32) (w: u32) (s: state): state =
    s with dimensions = (w, h) with mode = false

entry step (s: state): state =
  let ((rng, img), n_frames) =
    if s.mode && s.n_frames > 0
    then (sample_frame_accum s, s.n_frames + 1)
    else (sample_frame s, 1)
  in s with img = img
       with rng = rng
       with n_frames = n_frames

entry key (e: i32) (key: i32) (s: state): state =
  let keydown = e == 0 in
  if keydown
  then if key == SDLK_2
       then s with subsampling = s.subsampling + 1
              with n_frames = 0
       else if key == SDLK_1
       then s with subsampling = u32.max 1 (s.subsampling - 1)
              with n_frames = 0
       else if key == SDLK_w
       then s with cam = move_camera s.cam (mkvec3 0 0 1) with n_frames = 0
       else if key == SDLK_a
       then s with cam = move_camera s.cam (mkvec3 (-1) 0 0) with n_frames = 0
       else if key == SDLK_s
       then s with cam = move_camera s.cam (mkvec3 0 0 (-1)) with n_frames = 0
       else if key == SDLK_d
       then s with cam = move_camera s.cam (mkvec3 1 0 0) with n_frames = 0
       else if key == SDLK_UP
       then s with cam = turn_camera s.cam (-0.1) 0.0 with n_frames = 0
       else if key == SDLK_DOWN
       then s with cam = turn_camera s.cam 0.1 0.0 with n_frames = 0
       else if key == SDLK_RIGHT
       then s with cam = turn_camera s.cam 0.0 0.1 with n_frames = 0
       else if key == SDLK_LEFT
       then s with cam = turn_camera s.cam 0.0 (-0.1) with n_frames = 0
       else if key == SDLK_x
       then s with cam = move_camera s.cam (mkvec3 0 1 0) with n_frames = 0
       else if key == SDLK_z
       then s with cam = move_camera s.cam (mkvec3 0 (-1) 0) with n_frames = 0
       else if key == SDLK_SPACE
       then s with mode = !s.mode
              with n_frames = 0
       else if key == SDLK_n
       then s with mode = false with n_frames = 0
       else if key == SDLK_m
       then s with mode = true
       else if key == SDLK_i
       then s with cam =
         (s.cam with conf.aperture = f32.min 2 (s.cam.conf.aperture + 0.08))
       else if key == SDLK_k
       then s with cam =
         (s.cam with conf.aperture = f32.max 0 (s.cam.conf.aperture - 0.08))
       else if key == SDLK_o
       then s with cam =
         (s.cam with conf.focal_dist = s.cam.conf.focal_dist * 1.14)
       else if key == SDLK_l
       then s with cam =
         (s.cam with conf.focal_dist = f32.max 0.1 (s.cam.conf.focal_dist / 1.14))
       else if key == SDLK_t
       then (match s.cam_conf_id
             case 0 -> s with cam.conf = visual_flash_conf
                         with cam_conf_id = 1
                         with render_mode = #render_color
             case 1 -> s with cam.conf = lidar_conf
                         with cam_conf_id = 2
                         with render_mode = #render_distance
             case _ -> s with cam.conf = visual_conf
                         with cam_conf_id = 0
                         with render_mode = #render_color
            ) with n_frames = 0
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


-- TODO: Play with constants for parallelism saturation
--
-- Set default number of groups
-- Set group size
