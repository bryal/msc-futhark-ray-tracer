import "linalg"
import "common"
import "rand"

type camera = { pitch: f32
              , yaw: f32
              , origin: vec3
              , aperture: f32
              , focal_dist: f32 }

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
