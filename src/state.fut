import "linalg"
import "camera"
import "rand"
import "scene"

type render_mode = #render_distance | #render_color

type~ state =
  { dimensions: (u32, u32)
  , subsampling: u32
  , rng: minstd_rand.rng
  , img: [][]vec3
  , n_frames: u32
  , ambience: spectrum
  , mode: bool
  , render_mode: render_mode
  , cam_conf_id: u32
  , cam: camera
  , scene: accel_scene }
