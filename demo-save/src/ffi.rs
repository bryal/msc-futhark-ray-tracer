#[link(name = "tracer", kind = "static")]
extern "C" {
    pub type futhark_context;
    pub type futhark_opaque_state;
    pub type futhark_u32_1d;
    pub type futhark_f32_1d;
    pub type futhark_f32_2d;
    pub type futhark_f32_3d;
    pub type futhark_f32_4d;
    pub type futhark_context_config;

    pub fn futhark_context_config_new() -> *mut futhark_context_config;

    pub fn futhark_context_new(cfg: *mut futhark_context_config) -> *mut futhark_context;

    pub fn futhark_new_u32_1d(
        ctx: *mut futhark_context,
        data: *mut u32,
        dim0: i64,
    ) -> *mut futhark_u32_1d;

    pub fn futhark_new_f32_1d(
        ctx: *mut futhark_context,
        data: *mut f32,
        dim0: i64,
    ) -> *mut futhark_f32_1d;

    pub fn futhark_new_f32_2d(
        ctx: *mut futhark_context,
        data: *mut f32,
        dim0: i64,
        dim1: i64,
    ) -> *mut futhark_f32_2d;

    pub fn futhark_new_f32_3d(
        ctx: *mut futhark_context,
        data: *mut f32,
        dim0: i64,
        dim1: i64,
        dim2: i64,
    ) -> *mut futhark_f32_3d;

    pub fn futhark_values_f32_3d(
        ctx: *mut futhark_context,
        arr: *mut futhark_f32_3d,
        out: *mut f32,
    ) -> i32;

    pub fn futhark_values_f32_4d(
        ctx: *mut futhark_context,
        arr: *mut futhark_f32_4d,
        out: *mut f32,
    ) -> i32;

    pub fn futhark_free_opaque_state(
        ctx: *mut futhark_context,
        state: *mut futhark_opaque_state,
    ) -> i32;

    pub fn futhark_entry_init(
        ctx: *mut futhark_context,
        out: *mut *mut futhark_opaque_state,
        seed: u32,
        h: u32,
        w: u32,
        cam_conf_id: u32,
        tri_geoms: *const futhark_f32_3d,
        tri_mats: *const futhark_u32_1d,
        mat_data: *const futhark_f32_2d,
        cam_pitch: f32,
        cam_yaw: f32,
        cam_origin: *const futhark_f32_1d,
    ) -> i32;

    // pub fn futhark_entry_sample_frame_(
    //     ctx: *mut futhark_context,
    //     out: *mut *mut futhark_f32_3d,
    //     state: *const futhark_opaque_state,
    // ) -> i32;

    // pub fn futhark_entry_sample_n_frames(
    //     ctx: *mut futhark_context,
    //     out: *mut *mut futhark_f32_3d,
    //     state: *const futhark_opaque_state,
    //     n: u32,
    // ) -> i32;

    // pub fn futhark_entry_sample_pixels_(
    //     ctx: *mut futhark_context,
    //     distances: *mut *mut futhark_f32_3d,
    //     channels: *mut *mut futhark_i32_3d,
    //     intensities: *mut *mut futhark_f32_3d,
    //     in0: *const futhark_opaque_state,
    // ) -> i32;

    pub fn futhark_entry_sample_points_n(
        ctx: *mut futhark_context,
        out_state: *mut *mut futhark_opaque_state,
        out_points: *mut *mut futhark_f32_3d,
        state: *const futhark_opaque_state,
        samples_per_pixle: u32,
    ) -> i32;
}
