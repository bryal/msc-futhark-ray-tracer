import "shapes"
import "spectrum"

type diffuselight = { geom: geom, emission: spectrum }
type frustumlight = { geom: geom, theta: angle, emission: spectrum}

type arealight = #diffuselight diffuselight
               | #frustumlight frustumlight

type light = #pointlight { pos: vec3, emission: spectrum }
           | #arealight arealight


let arealight_geom (l: arealight): geom =
  match l
  case #diffuselight x -> x.geom
  case #frustumlight x -> x.geom

let trianglelight_incident_radiance (emission: spectrum)
                                    (t: triangle)
                                    (hitp: vec3)
                                    (lightp: vec3)
                                    (wavelen: f32)
                                  : f32 =
  let (wi, distance_sq) =
    let v = lightp vec3.- hitp
    in (vec3.normalise v, vec3.quadrance v)
  let lnormal = triangle_normal t
  let cos_theta_l = vec3.dot (vec3_neg wi) lnormal
  in f32.max 0
             (spectrum_lookup wavelen emission * cos_theta_l / distance_sq)

let diffuselight_incident_radiance (l: diffuselight)
                                   (hitp: vec3)
                                   (lightp: vec3)
                                   (wavelen: f32)
                                 : f32 =
  match l.geom
  case #triangle t ->
    trianglelight_incident_radiance l.emission t hitp lightp wavelen
  -- TODO
  case #sphere _ -> 0

let frustumlight_incident_radiance (l: frustumlight)
                                   (hitp: vec3)
                                   (lightp: vec3)
                                   (wavelen: f32)
                                 : f32 =
  match l.geom
  case #triangle t ->
      let (wi, distance_sq) =
        let v = lightp vec3.- hitp
        in (vec3.normalise v, vec3.quadrance v)
      let lnormal = triangle_normal t
      let cos_theta_l = vec3.dot (vec3_neg wi) lnormal
      in if f32.acos cos_theta_l <= to_rad l.theta
         then spectrum_lookup wavelen l.emission / distance_sq
         else 0
  -- TODO
  case #sphere _ -> 0

let arealight_incident_radiance (l: arealight)
                                (hitp: vec3)
                                (lightp: vec3)
                                (wavelen: f32)
                              : f32 =
  match l
  case #diffuselight dl ->
    diffuselight_incident_radiance dl hitp lightp wavelen
  case #frustumlight fl ->
    frustumlight_incident_radiance fl hitp lightp wavelen
