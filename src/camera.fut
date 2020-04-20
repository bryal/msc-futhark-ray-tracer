import "../lib/github.com/diku-dk/statistics/statistics"
import "linalg"
import "common"
import "rand"
import "shapes"
import "spectrum"
import "light"


module stat = mk_statistics f32

type normal_dist = { mu: f32, sigma: f32 }

type~ sensor = [](normal_dist, vec3)

type transmitter = #flash { radius: f32, emission: spectrum }
                 | #scanning { radius: f32, theta: angle, emission: spectrum }
                 | #none

type~ camera = { pitch: f32
               , yaw: f32
               , origin: vec3
               , aperture: f32
               , focal_dist: f32
               , offset_radius: f32
               , sensor: sensor
               , transmitter: transmitter }

let cam_dir (cam: camera): vec3 =
  vec3.normalise
    (mkvec3 (f32.sin cam.yaw) (f32.sin cam.pitch) (-(f32.cos cam.yaw)))

let cam_right (cam: camera): vec3 =
  vec3.normalise (vec3.cross (cam_dir cam) world_up)

let cam_up (cam: camera): vec3 =
  vec3.normalise (vec3.cross (cam_right cam) (cam_dir cam))

let move_camera (cam: camera) (m: vec3): camera =
  let cam_forward = vec3.normalise (cam_dir cam with y = 0)
  in cam with origin = cam.origin
                       vec3.+ vec3.scale (0.1*m.z) cam_forward
                       vec3.+ vec3.scale (0.1*m.x) (cam_right cam)
                       vec3.+ vec3.scale (0.1*m.y) world_up

let turn_camera (cam: camera) (pitch: f32) (yaw: f32): camera =
  cam with pitch = clamp (-0.5*f32.pi, 0.5*f32.pi) (cam.pitch + pitch)
      with yaw = (cam.yaw + yaw) % (2*f32.pi)

let sample_camera_wavelength (cam: camera) (rng: rnge)
                           : (rnge, f32, vec3) =
  -- TODO: Should probably not just be 1/n. The ration of area of
  --       distribution / area of all distributions?
  let (rng, (wavelen_distr, wavelen_radiance_to_rgb)) =
    random_select rng cam.sensor
  let wavelen_radiance_to_rgb =
    vec3.scale (f32.i32 (length cam.sensor)) wavelen_radiance_to_rgb
  -- Sample an `x` value (wavelength) from the normal distribution
  -- with inverse transform sampling of normal distribution
  -- (aka. probit / quantile function)
  let (rng, p) = random_unit_exclusive rng
  let wavelen = stat.sample (stat.mk_normal wavelen_distr) p
  in (rng, wavelen, wavelen_radiance_to_rgb)

let sample_camera_ray (cam: camera) (wh: vec2) (ji: vec2) (rng: rnge): ray =
  let ratio = wh.x / wh.y
  -- TODO: When lidar, don't apply random offset, and don't "stretch"
  --       the point to cover the whole pixel, so to speak.
  let { x, y } =
    let (_rng, offset) = random_in_unit_square rng
    let offset = vec2.scale cam.offset_radius (mkvec2 offset.0 offset.1)
    in (ji vec2.+ offset) vec2./ wh
  let lens_radius = cam.aperture / 2
  let field_of_view = from_deg 80.0
  let half_height = f32.tan ((to_rad field_of_view) / 2.0)
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
            vec3.+ vec3.scale x horizontal
            vec3.+ vec3.scale y vertical
            vec3.- origin)

let gen_transmitter (c: camera) (r: ray): []light =
  let n_sectors = 8 in
  map (\l -> #arealight l)
  <| match c.transmitter
     case #flash { radius, emission } ->
       map (\t -> #diffuselight { geom = #triangle t, emission })
           (disk c.origin (cam_dir c) radius n_sectors)
     case #scanning { radius, theta, emission } ->
       map (\t -> #frustumlight { geom = #triangle t, theta, emission })
           (disk c.origin r.dir radius n_sectors)
     case #none -> []
