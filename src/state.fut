import "linalg"
import "camera"
import "rand"
import "scene"

type~ state = { time: f32
              , dimensions: (u32, u32)
              , subsampling: u32
              , rng: minstd_rand.rng
              , img: [][]vec3
              , samples: u32
              , n_frames: u32
              , ambience: spectrum
              , mode: bool
              , cam: camera
              , lidar_mode: bool
              , scene: accel_scene }
