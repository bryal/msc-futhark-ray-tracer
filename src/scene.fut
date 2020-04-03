import "shapes"
import "material"
import "light"
import "bvh"

type obj = { geom: geom, mat_ix: u32 }

module obj_Geom: Geom with t = obj = {
  type t = obj
  let get_geom: obj -> geom = (.geom)
}

module obj_bvh = mk_lbvh obj_Geom
type~ obj_bvh = obj_bvh.bvh

type~ scene =
  { objs: []obj
  , mats: []material }

type~ accel_scene =
  { objs: obj_bvh
  , mats: []material
  , lights: []light }


let parse_triangles [t]
                    (tris: [t][3][3]f32) (tri_mats: [t]u32)
                  : [t]obj =
  let f tri (mat_ix: u32) =
    let tri' = (map vec3_from_array tri)
    in { geom = #triangle { a = tri'[0]
                          , b = tri'[1]
                          , c = tri'[2] }
       , mat_ix }
  in map2 f tris tri_mats

let parse_mat (m: [28]f32): material =
  { color = { b0 = (m[0] , m[1])
            , b1 = (m[2] , m[3])
            , b2 = (m[4] , m[5])
            , b3 = (m[6] , m[7])
            , b4 = (m[8] , m[9])
            , b5 = (m[10] , m[11]) }
  , roughness = m[12]
  , metalness = m[13]
  , ref_ix = m[14]
  , opacity = m[15]
  , emission = { b0 = (m[16] , m[17])
               , b1 = (m[18] , m[19])
               , b2 = (m[20] , m[21])
               , b3 = (m[22] , m[23])
               , b4 = (m[24] , m[25])
               , b5 = (m[26] , m[27]) } }

let parse_mats (mats: [][28]f32): []material =
  map parse_mat mats

let get_lights ({ objs, mats }: scene): []light =
  let nonzero_spectrum s = !(null (filter (\(w, x) -> w >= 0 && x > 0)
                                          (spectrum_to_arr s)))
  let with_emission obj =
    { geom = obj.geom
    , emission = (unsafe mats[i32.u32 obj.mat_ix]).emission }
  in map (\l -> #arealight (#diffuselight l))
     <| filter (nonzero_spectrum <-< (.emission))
     <| map with_emission objs

let closest_interaction (tmax: f32) (lr: lightray) (ms: []material) (xs: obj_bvh)
                      : maybe interaction =
  maybe.map (\(o, h) -> { h
                        , mat = unsafe ms[i32.u32 o.mat_ix]
                        , wavelen = lr.wavelen })
            (obj_bvh.closest_hit tmax lr.r xs)

let accelerate_scene (s: scene): accel_scene =
  { objs = obj_bvh.build s.objs, mats = s.mats, lights = get_lights s }
