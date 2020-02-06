#![feature(vec_into_raw_parts)]

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
    vec![
        (-1.0),
        0.0,
        (-2.2),
        0.0,
        0.0,
        (-2.2),
        (-0.5),
        0.5,
        (-2.3),
        0.0,
        0.0,
        (-2.2),
        1.0,
        0.0,
        (-2.2),
        0.5,
        0.5,
        (-2.3),
        (-0.5),
        0.5,
        (-2.3),
        0.5,
        0.5,
        (-2.3),
        0.0,
        2.0,
        (-2.4),
    ]
}
