import "lys/lys"

import "material"
import "bvh"
import "camera"

module xbvh = lbvh

let mkray (o: vec3) (d: vec3): ray =
  { origin = o, dir = vec3.normalise(d) }

type~ state = { time: f32
              , dimensions: (u32, u32)
              , subsampling: u32
              , rng: minstd_rand.rng
              , img: [][]vec3
              , samples: u32
              , n_frames: u32
              , mode: bool
              , cam: camera
              , mats: []material
              , world: []obj }

let vcol_to_argb (c: vec3): argb.colour =
  argb.from_rgba c.x c.y c.z 1f32

let advance_rng (rng: rnge): rnge =
  let (rng, _) = dist.rand (0,1) rng in rng

let mkray_adjust_acne (h: hit) (wi: vec3): ray =
  -- Fix surface acne
  --
  -- NOTE: Don't just walk along `wi`, because then we get
  -- strange artifacts for some materials at extreme angles.
  let eps = 0.001
  let face_forward_normal = if vec3.dot wi h.normal >= 0
                            then h.normal
                            else vec3_neg h.normal
  let acne_offset = vec3.scale eps face_forward_normal
  in mkray (h.pos vec3.+ acne_offset) wi

let color (r: ray) (world: xbvh.bvh) (mats: []material) (rng: rnge)
        : vec3 =
  let tmax = f32.highest
  let sky = mkvec3 0.8 0.9 1.0
  -- Choke throughput to end the loop, returning the radiance
  let finish radiance =
    let choked_throughput = mkvec3 0 0 0
    let (arbitrary_ray, arbitrary_rng) = (r, rng)
    in (radiance, choked_throughput, arbitrary_ray, arbitrary_rng)
  -- Russian roulette termination. Instead of absolutely cutting off
  -- the "recursion" after N bounces, keep going with some probability
  -- and weight the samples appropriately. When we do it like this,
  -- the final result is an unbiased estimate of the sum. See PBR Book
  -- 14.5.1.
  let p_termination = 0.1
  let roulette_terminate rng = (random_unit_exclusive rng).1 < p_termination
  in (.0) <|
     loop (radiance, throughput, r, rng) =
          (mkvec3 0 0 0, mkvec3 1 1 1, r, rng)
     while vec3.norm throughput > 0.001 && !(roulette_terminate rng)
     do let throughput = vec3.scale (1 / (1 - p_termination)) throughput
        in match xbvh.closest_hit tmax r mats world
           case #just h ->
             let rng = advance_rng rng
             let radiance = radiance vec3.+ (throughput vec3.* h.mat.emission)
             let wo = vec3_neg r.dir
             let (rng, { wi, bsdf, pdf }) = sample_dir wo h rng
             let cosFalloff = f32.abs (vec3.dot h.normal wi)
             let throughput = throughput vec3.* (vec3.scale (cosFalloff / pdf) bsdf)
             let r = mkray_adjust_acne h wi
             in if pdf == 0
                then finish (mkvec3 0 0 0)
                else (radiance, throughput, r, rng)
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

let sample (world: xbvh.bvh)
           (cam: camera)
           (mats: []material)
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
  in color r world mats rng

let sample_all (s: state): (rnge, [][]vec3) =
  let world_bvh = xbvh.build s.world
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
    in (vec3./) (sample world_bvh
                        s.cam
                        s.mats
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
    { time = 0
    , dimensions = (w, h)
    , subsampling = 2
    , rng = minstd_rand.rng_from_seed [123]
    , img = tabulate_2d (i32.u32 h) (i32.u32 w) (\_ _ -> mkvec3 0 0 0)
    , samples = 1
    , n_frames = 1
    , mode = false
    , cam = { pitch = 0.0, yaw = 0.0
            , origin = mkvec3 0 0.8 1.8
            , aperture = 0.0, focal_dist = 1.5 }
    , mats = parse_mats mat_data
    , world = parse_triangles tri_geoms tri_mats }

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

  let text_content (render_duration: f32) (s: state): text_content =
    let rd = if s.mode then 0 else u32.f32 render_duration
    in ( rd, s.samples, s.n_frames , s.cam.aperture, s.cam.focal_dist,
      s.subsampling )

  let text_colour = const argb.yellow
}
