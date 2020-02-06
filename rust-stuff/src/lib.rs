#![feature(vec_into_raw_parts)]

use std::path::Path;

#[no_mangle]
pub extern "C" fn load_obj_data(num: *mut usize, data: *mut *mut f32) {
    let (ptr, len, _) = load().into_raw_parts();
    unsafe {
        *num = len;
        *data = ptr
    }
}

#[no_mangle]
pub unsafe extern "C" fn free_obj_data(data: *mut f32) {
    Box::from_raw(data);
}

fn load() -> Vec<f32> {
    let (models, _mats) =
        tobj::load_obj(Path::new("assets/CornellBox-Original.obj")).expect("Load cornell box");
    models
        .into_iter()
        .flat_map(|m| mesh_to_triangle_data(m.mesh))
        .collect()
}

fn mesh_to_triangle_data(m: tobj::Mesh) -> Vec<f32> {
    let vs = m.positions.chunks(3).collect::<Vec<_>>();
    m.indices
        .into_iter()
        .flat_map(|i| vs[i as usize])
        .cloned()
        .collect()
}
