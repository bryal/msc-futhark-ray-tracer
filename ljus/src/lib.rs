#![feature(vec_into_raw_parts)]

use std::ffi::CStr;
use std::iter::{once, repeat};
use std::path::Path;

const RED_WAVELEN: f32 = 610.0;
const GREEN_WAVELEN: f32 = 550.0;
const BLUE_WAVELEN: f32 = 460.0;

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

pub fn load(obj_path: &Path) -> (Vec<f32>, Vec<u32>, Vec<f32>) {
    let (models, materials) = tobj::load_obj(obj_path).expect("Load obj file");
    let (mut tris, mut tri_mats) = (Vec::new(), Vec::new());
    for mesh in models.into_iter().map(|m| m.mesh) {
        let mat_ix = mesh.material_id.expect("Mesh doesn't have material");
        let vertices = mesh.positions.chunks(3).collect::<Vec<_>>();
        for tri_is in mesh.indices.chunks(3) {
            tri_mats.push(mat_ix as u32);
            for &v_i in tri_is {
                tris.extend(vertices[v_i as usize].iter().cloned());
            }
        }
    }
    let mut mats = Vec::with_capacity(materials.len());
    for m in materials {
        let color_old = m.diffuse;
        let color = get_spectrum(&m, "Sp").unwrap_or([
            RED_WAVELEN,
            color_old[0],
            GREEN_WAVELEN,
            color_old[1],
            BLUE_WAVELEN,
            color_old[2],
            -1.0,
            0.0,
            -1.0,
            0.0,
            -1.0,
            0.0,
        ]);
        let roughness = get_scalar(&m, "Pr").unwrap_or(1.0);
        let metalness = get_scalar(&m, "Pm").unwrap_or(0.0);
        let ref_ix = m.optical_density;
        let opacity = get_scalar(&m, "Tf").unwrap_or(1.0);
        let emission_old = get_vec3(&m, "Ke").unwrap_or([0.0, 0.0, 0.0]);
        let emission = get_spectrum(&m, "Em").unwrap_or([
            RED_WAVELEN,
            emission_old[0],
            GREEN_WAVELEN,
            emission_old[1],
            BLUE_WAVELEN,
            emission_old[2],
            -1.0,
            0.0,
            -1.0,
            0.0,
            -1.0,
            0.0,
        ]);
        let mut mat = [0.0; 28];
        let mat_it = color
            .iter()
            .chain(once(&roughness))
            .chain(once(&metalness))
            .chain(once(&ref_ix))
            .chain(once(&opacity))
            .chain(emission.iter());
        for (i, &x) in mat_it.enumerate() {
            mat[i] = x
        }
        mats.push(mat);
    }
    println!("no of triangles: {:?}", tris.len() / 9);
    (tris, tri_mats, mats.concat())
}

fn parse_scalar(s: &str) -> f32 {
    s.parse::<f32>().expect("Scalar parameter")
}

fn parse_vec(s: &str) -> Vec<f32> {
    s.split_whitespace()
        .map(|t| t.parse::<f32>().unwrap())
        .collect::<Vec<_>>()
}

fn parse_vec3(s: &str) -> [f32; 3] {
    let v = parse_vec(s);
    if v.len() != 3 {
        panic!("Expected 3-vector parameter")
    } else {
        [v[0], v[1], v[2]]
    }
}

fn get_scalar(m: &tobj::Material, field: &str) -> Option<f32> {
    m.unknown_param.get(field).map(|s| parse_scalar(&s))
}

fn get_vec3(m: &tobj::Material, field: &str) -> Option<[f32; 3]> {
    m.unknown_param.get(field).map(|s| parse_vec3(&s))
}

fn get_spectrum(m: &tobj::Material, field: &str) -> Option<[f32; 12]> {
    m.unknown_param.get(field).map(|s| {
        let v = parse_vec(s);
        let filler = repeat(&[-1.0, 0.0]).flat_map(|x| x);
        let mut v12 = [0.0; 12];
        for (i, &x) in v.iter().chain(filler).take(12).enumerate() {
            v12[i] = x;
        }
        v12
    })
}
