#![feature(extern_types)]
#![feature(vec_into_raw_parts)]
#![feature(clamp)]

mod ffi;
mod wrapper;

use pcd_rs::{DataKind, WriterBuilder};
use wrapper::*;

fn main() {
    let dims: (u32, u32) = (640, 480);
    let cam = Cam {
        origin: Vec3 {
            x: 0.0,
            y: 0.8,
            z: 1.8,
        },
        pitch: 0.0,
        yaw: 0.0,
    };
    let mut fut = Fut::init(dims, cam, "assets/SpectrumSphere.obj");
    let points = fut.sample_points(100);
    let mut writer =
        WriterBuilder::new(points.len() as u64, 1, Default::default(), DataKind::ASCII)
            .expect("new WriterBuilder")
            .create::<_, Vec3>("dump.pcd")
            .expect("created path");
    for point in points.iter() {
        writer.push(&point).unwrap();
    }
    writer.finish().unwrap();

    // unsafe {
    // Capturing and saving an image
    //
    // let mut fut_mat: *mut futhark_f32_3d = mem::zeroed();
    // futhark_entry_sample_n_frames(ctx, &mut fut_mat, state, 100);
    //
    // let mut data = vec![0f32; (width * height) as usize * 3];
    // futhark_values_f32_3d(ctx, fut_mat, data.as_mut_ptr());
    //
    // let bytes = data
    //     .into_iter()
    //     .map(|x| (x.clamp(0.0, 1.0) * 255.99) as u8)
    //     .collect::<Vec<u8>>();
    //
    // image::save_buffer("out.png", &bytes, width, height, image::ColorType::Rgb8).unwrap();
    // }
}
