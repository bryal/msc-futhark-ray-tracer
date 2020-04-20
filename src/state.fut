import "linalg"
import "camera"
import "rand"
import "scene"

type render_mode = #render_distance | #render_color

type~ state =
  { time: f32
  , dimensions: (u32, u32)
  , subsampling: u32
  , rng: minstd_rand.rng
  , samples: u32
  , img: [][]vec3
  , n_frames: u32
  , ambience: spectrum
  , mode: bool
  , render_mode: render_mode
  , cam: camera
  , scene: accel_scene }
