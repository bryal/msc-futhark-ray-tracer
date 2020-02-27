#![feature(vec_into_raw_parts)]

use std::ffi::CStr;
use std::path::Path;

#[no_mangle]
pub extern "C" fn load_obj_data(
    obj_path: *const i8,
    num_tris: *mut usize,
    num_mat_components: *mut usize,
    tri_data: *mut *mut f32,
    tri_mats: *mut *mut u32,
    mat_data: *mut *mut f32,
) {
    unsafe {
        let obj_path = Path::new(CStr::from_ptr(obj_path).to_str().unwrap());
        let (ts, tms, ms) = load(obj_path);
        let (ts_ptr, _, _) = ts.into_raw_parts();
        let (tm_ptr, n_tris, _) = tms.into_raw_parts();
        let (ms_ptr, n_mat_components, _) = ms.into_raw_parts();
        *num_tris = n_tris;
        *num_mat_components = n_mat_components;
        *tri_data = ts_ptr;
        *tri_mats = tm_ptr;
        *mat_data = ms_ptr;
    }
}

#[no_mangle]
pub unsafe extern "C" fn free_obj_data(tri_data: *mut f32, tri_mats: *mut u32, mat_data: *mut f32) {
    Box::from_raw(tri_data);
    Box::from_raw(tri_mats);
    Box::from_raw(mat_data);
}

fn load(obj_path: &Path) -> (Vec<f32>, Vec<u32>, Vec<f32>) {
    let (models, materials) = tobj::load_obj(obj_path).expect("Load obj file");
    let error_mat_ix = 0;
    let (mut tris, mut tri_mats) = (Vec::new(), Vec::new());
    for mesh in models.into_iter().map(|m| m.mesh) {
        let mat_ix = mesh.material_id.map(|i| i + 1).unwrap_or(error_mat_ix);
        let vertices = mesh.positions.chunks(3).collect::<Vec<_>>();
        for tri_is in mesh.indices.chunks(3) {
            tri_mats.push(mat_ix as u32);
            for &v_i in tri_is {
                tris.extend(vertices[v_i as usize].iter().cloned());
            }
        }
    }
    let error_mat = [1000.0, 0.0, 1000.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0];
    let mut mats = vec![error_mat];
    for m in materials {
        let color = m.diffuse;
        let roughness = get_scalar(&m, "Pr");
        let metalness = get_scalar(&m, "Pm");
        let ref_ix = m.optical_density;
        let emission = get_vec3(&m, "Ke");
        let mat = [
            color[0],
            color[1],
            color[2],
            roughness,
            metalness,
            ref_ix,
            emission[0],
            emission[1],
            emission[2],
        ];
        mats.push(mat);
    }
    println!("no of triangles: {:?}", tris.len() / 9);
    (tris, tri_mats, mats.concat())
}

fn parse_scalar(s: &str) -> f32 {
    s.parse::<f32>().expect("Scalar parameter")
}

fn parse_vec3(s: &str) -> [f32; 3] {
    let v = s
        .split_whitespace()
        .map(|t| t.parse::<f32>().unwrap())
        .collect::<Vec<_>>();
    if v.len() != 3 {
        panic!("Expected 3-vector parameter")
    } else {
        [v[0], v[1], v[2]]
    }
}

fn get_scalar(m: &tobj::Material, field: &str) -> f32 {
    m.unknown_param
        .get(field)
        .map(|s| parse_scalar(&s))
        .unwrap_or(0.0)
}

fn get_vec3(m: &tobj::Material, field: &str) -> [f32; 3] {
    m.unknown_param
        .get(field)
        .map(|s| parse_vec3(&s))
        .unwrap_or([0.0, 0.0, 0.0])
}
