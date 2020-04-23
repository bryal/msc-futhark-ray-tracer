#![feature(extern_types)]
#![feature(vec_into_raw_parts)]
#![feature(clamp)]

extern crate image;
extern crate ljus;

use std::mem;

#[link(name = "tracer", kind = "static")]
extern "C" {
    type futhark_context;
    type futhark_opaque_state;
    type futhark_u32_1d;
    type futhark_f32_1d;
    type futhark_f32_2d;
    type futhark_f32_3d;
    type futhark_context_config;

    fn futhark_context_config_new() -> *mut futhark_context_config;

    fn futhark_context_new(cfg: *mut futhark_context_config) -> *mut futhark_context;

    fn futhark_new_u32_1d(
        ctx: *mut futhark_context,
        data: *mut u32,
        dim0: i64,
    ) -> *mut futhark_u32_1d;

    fn futhark_new_f32_1d(
        ctx: *mut futhark_context,
        data: *mut f32,
        dim0: i64,
    ) -> *mut futhark_f32_1d;

    fn futhark_new_f32_2d(
        ctx: *mut futhark_context,
        data: *mut f32,
        dim0: i64,
        dim1: i64,
    ) -> *mut futhark_f32_2d;

    fn futhark_new_f32_3d(
        ctx: *mut futhark_context,
        data: *mut f32,
        dim0: i64,
        dim1: i64,
        dim2: i64,
    ) -> *mut futhark_f32_3d;

    fn futhark_entry_init(
        ctx: *mut futhark_context,
        out: *mut *mut futhark_opaque_state,
        seed: u32,
        h: u32,
        w: u32,
        samples_per_pixel: u32,
        cam_conf_id: u32,
        tri_geoms: *const futhark_f32_3d,
        tri_mats: *const futhark_u32_1d,
        mat_data: *const futhark_f32_2d,
        cam_pitch: f32,
        cam_yaw: f32,
        cam_origin: *const futhark_f32_1d,
    ) -> i32;

    fn futhark_entry_sample_pixels_visualize_(
        ctx: *mut futhark_context,
        out: *mut *mut futhark_f32_3d,
        state: *const futhark_opaque_state,
    ) -> i32;

    fn futhark_values_f32_3d(
        ctx: *mut futhark_context,
        arr: *mut futhark_f32_3d,
        out: *mut f32,
    ) -> i32;
}

fn main() {
    let (width, height): (u32, u32) = (640, 480);

    unsafe {
        let cfg = futhark_context_config_new();
        let ctx = futhark_context_new(cfg);

        let obj_path = std::path::Path::new("assets/SpectrumSphere.obj");
        let (tris, tri_mats, mats) = ljus::load(obj_path);
        let (tri_data, _, _) = tris.into_raw_parts();
        let (tri_mats, n_tris, _) = tri_mats.into_raw_parts();
        let (mat_data, n_mat_components, _) = mats.into_raw_parts();

        let fut_tri_data = futhark_new_f32_3d(ctx, tri_data, n_tris as i64, 3, 3);
        let fut_tri_mats = futhark_new_u32_1d(ctx, tri_mats, n_tris as i64);
        let fut_mat_data = futhark_new_f32_2d(ctx, mat_data, n_mat_components as i64 / 28, 28);

        let cam_pitch: f32 = 0.0;
        let cam_yaw: f32 = 0.0;
        let mut cam_origin = [0.0, 0.8, 1.8];
        let cam_origin = futhark_new_f32_1d(ctx, cam_origin.as_mut_ptr(), 3);
        let mut state: *mut futhark_opaque_state = mem::zeroed();
        let samples_per_pixel = 4;
        let cam_conf_id = 2;
        futhark_entry_init(
            ctx,
            &mut state,
            0,
            height,
            width,
            samples_per_pixel,
            cam_conf_id,
            fut_tri_data,
            fut_tri_mats,
            fut_mat_data,
            cam_pitch,
            cam_yaw,
            cam_origin,
        );

        let mut fut_mat: *mut futhark_f32_3d = mem::zeroed();
        futhark_entry_sample_pixels_visualize_(ctx, &mut fut_mat, state);

        let mut data = vec![0f32; (width * height) as usize * 3];
        futhark_values_f32_3d(ctx, fut_mat, data.as_mut_ptr());

        let bytes = data
            .into_iter()
            .map(|x| (x.clamp(0.0, 1.0) * 255.99) as u8)
            .collect::<Vec<u8>>();

        image::save_buffer("out.png", &bytes, width, height, image::ColorType::Rgb8).unwrap();
    }
}
