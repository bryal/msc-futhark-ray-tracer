#![feature(vec_into_raw_parts)]

use std::ffi::CStr;
use std::path::Path;

#[no_mangle]
pub extern "C" fn load_obj_data(obj_path: *const i8, num: *mut usize, data: *mut *mut f32) {
    unsafe {
        let obj_path = Path::new(CStr::from_ptr(obj_path).to_str().unwrap());
        let (ptr, len, _) = load(obj_path).into_raw_parts();
        *num = len;
        *data = ptr
    }
}

#[no_mangle]
pub unsafe extern "C" fn free_obj_data(data: *mut f32) {
    Box::from_raw(data);
}

fn load(obj_path: &Path) -> Vec<f32> {
    let (models, _mats) = tobj::load_obj(obj_path).expect("Load cornell box");
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
