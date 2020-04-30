use crate::ffi::*;
use pcd_rs::{PcdDeserialize, PcdSerialize};
use std::mem;
use std::path::Path;

#[repr(C)]
struct Point {
    pos: Vec3,
    intensity: f32,
}

#[derive(Clone, Copy, Debug, PcdDeserialize, PcdSerialize, PartialEq)]
#[repr(C)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

pub struct Cam {
    pub origin: Vec3,
    pub pitch: f32,
    pub yaw: f32,
}

pub struct Fut {
    ctx: *mut futhark_context,
    state: *mut futhark_opaque_state,
    width: u32,
    height: u32,
}

impl Fut {
    pub fn init<P>((width, height): (u32, u32), cam: Cam, obj_path: P) -> Self
    where
        P: AsRef<Path>,
    {
        unsafe {
            let cfg = futhark_context_config_new();
            let ctx = futhark_context_new(cfg);
            let (tris, tri_mats, mats) = ljus::load(obj_path.as_ref());
            let (tri_data, _, _) = tris.into_raw_parts();
            let (tri_mats, n_tris, _) = tri_mats.into_raw_parts();
            let (mat_data, n_mat_components, _) = mats.into_raw_parts();
            let fut_tri_data = futhark_new_f32_3d(ctx, tri_data, n_tris as i64, 3, 3);
            let fut_tri_mats = futhark_new_u32_1d(ctx, tri_mats, n_tris as i64);
            let fut_mat_data = futhark_new_f32_2d(ctx, mat_data, n_mat_components as i64 / 28, 28);
            let mut cam_origin = [cam.origin.x, cam.origin.y, cam.origin.z];
            let cam_origin = futhark_new_f32_1d(ctx, cam_origin.as_mut_ptr(), 3);
            let cam_conf_id = 2;
            let mut state: *mut futhark_opaque_state = mem::zeroed();
            futhark_entry_init(
                ctx,
                &mut state,
                0,
                height,
                width,
                cam_conf_id,
                fut_tri_data,
                fut_tri_mats,
                fut_mat_data,
                cam.pitch,
                cam.yaw,
                cam_origin,
            );
            Self {
                ctx,
                state,
                width,
                height,
            }
        }
    }

    fn sample_points_(&mut self, samples_per_pixel: u32) -> Vec<Point> {
        unsafe {
            let mut new_state: *mut futhark_opaque_state = mem::zeroed();
            let mut fut_data: *mut futhark_f32_3d = mem::zeroed();
            futhark_entry_sample_points_n(
                self.ctx,
                &mut new_state,
                &mut fut_data,
                self.state,
                samples_per_pixel,
            );
            let old_state = mem::replace(&mut self.state, new_state);
            futhark_free_opaque_state(self.ctx, old_state);
            let mut data = vec![[0f32; 4]; self.width as usize * self.height as usize];
            futhark_values_f32_3d(self.ctx, fut_data, data.as_mut_ptr() as *mut _);
            mem::transmute::<Vec<[f32; 4]>, Vec<Point>>(data)
        }
    }

    // Capture a point cloud
    pub fn sample_points(&mut self, samples_per_pixel: u32) -> Vec<Vec3> {
        self.sample_points_(samples_per_pixel)
            .into_iter()
            .map(|p| p.pos)
            .collect()
    }
}
