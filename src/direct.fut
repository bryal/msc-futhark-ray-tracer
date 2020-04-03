import "shapes"
import "linalg"
import "scene"

type light_sample = { pos: vec3, wi: vec3, in_radiance: f32, pdf: f32 }


let occluded (h: hit) (lightp: vec3) (objs: obj_bvh)
           : bool =
  let v = lightp vec3.- h.pos
  let w = vec3.normalise v
  let eps = 0.01
  in vec3.dot w h.normal <= 0
     || (let distance = vec3.norm v
         let r = mkray_adjust_acne h w
         in obj_bvh.any_hit (distance - eps) r objs)

let triangle_area (t: triangle): f32 =
  let e1 = t.b vec3.- t.a
  let e2 = t.c vec3.- t.a
  in vec3.norm (vec3.cross e1 e2) / 2

let arealight_pdf (l: arealight): f32 =
  match arealight_geom l
  -- TODO
  case #sphere _ -> 0
  case #triangle t -> 1 / triangle_area t

let sample_pointlight (i: interaction) (pos: vec3) (emission: spectrum)
                    : light_sample =
  let (wi, distance_sq) =
    let v = pos vec3.- i.h.pos
    in (vec3.normalise v, vec3.quadrance v)
  let in_radiance = spectrum_lookup i.wavelen emission / distance_sq
  in { pos, wi, in_radiance, pdf = 1 }

let sample_arealight (rng: rnge) (i: interaction) (l: arealight)
                   : (rnge, light_sample) =
  match arealight_geom l
  case #triangle t ->
    let e1 = t.b vec3.- t.a
    let e2 = t.c vec3.- t.a
    let area = vec3.norm (vec3.cross e1 e2) / 2
    let (_rng, (u, v)) = random_in_triangle rng
    let p = vec3.(t.a + scale u e1 + scale v e2)
    let wi = vec3.normalise (p vec3.- i.h.pos)
    let in_radiance = arealight_incident_radiance l i.h.pos p i.wavelen
    in (rng, { pos = p, wi, in_radiance, pdf = 1 / area })
  -- TODO
  case #sphere _ ->
    (rng, { pos = mkvec3 0 0 0
          , wi = mkvec3 0 0 0
          , in_radiance = 0
          , pdf = 0 })

let sample_light (rng: rnge) (i: interaction) (l: light) (objs: obj_bvh)
               : (rnge, light_sample) =
  let (rng, light_sample) =
    match l
    case #pointlight { pos, emission } ->
      (rng, sample_pointlight i pos emission)
    case #arealight a -> sample_arealight rng i a
  in (rng, if occluded i.h light_sample.pos objs
           then light_sample with in_radiance = 0
           else light_sample)

-- The Balance heuristic of Multiple Importance Sampling
let balance (nf: u32, pdf_f: f32) (ng: u32, pdf_g: f32): f32 =
  let (nf, ng) = (f32.u32 nf, f32.u32 ng)
  in nf * pdf_f / (nf * pdf_f + ng * pdf_g)

-- TODO: Oh jeez we gotta make this more readable 100%
--
-- Estimate direct light contribution using Multiple Importance Sampling.
let estimate_direct (rng: rnge)
                    (wo: vec3)
                    (i: interaction)
                    (l: light)
                    (objs: obj_bvh)
                  : (rnge, f32) =
  -- Sample light with MIS
  let (rng, light_radiance) =
    let (rng, { pos = _, wi, in_radiance, pdf }) =
      sample_light rng i l objs
    in if pdf == 0 || in_radiance == 0
       then (rng, 0)
       else let f = bsdf_f wo wi i * f32.abs (vec3.dot wi i.h.normal)
            let scattering_pdf = bsdf_pdf wo wi i
            let weight = balance (1, pdf) (1, scattering_pdf)
            in (rng, f * weight * in_radiance / pdf)
  -- Sample BSDF with MIS
  let (rng, bsdf_radiance) =
    match l
    case #pointlight _ -> (rng, 0)
    case #arealight l ->
      let (rng, { wi, bsdf, pdf }) = sample_dir wo i rng
      in ( rng
         , let r = mkray_adjust_acne i.h wi
           in match hit_geom f32.highest r (arealight_geom l)
              case #nothing -> 0
              case #just lh ->
                if occluded i.h lh.pos objs
                then 0
                else let in_radiance =
                       arealight_incident_radiance l i.h.pos lh.pos i.wavelen
                     let f = bsdf * f32.abs (vec3.dot wi i.h.normal)
                     in match pdf
                        case #impossible -> 0
                        case #delta -> f * in_radiance
                        case #nonzero pdf ->
                          let light_pdf = arealight_pdf l
                          let weight =
                            balance (1, pdf) (1, light_pdf)
                          in f * in_radiance * weight / pdf)
  in (rng, light_radiance + bsdf_radiance)


-- Compute the direct radiance by stochastically sampling one light,
-- taking the reflection direction into account with Multiple
-- Importance Sampling to eliminate fireflies and get caustics.
--
-- Basically equivalent to `UniformSampleOneLight` of PBR Book 14.3.
let direct_radiance (rng: rnge)
                    (wo: vec3)
                    (i: interaction)
                    (scene: accel_scene)
                  : (rnge, f32) =
  if null scene.lights
  then (rng, 0)
  else let (rng, l) = random_select rng scene.lights
       let (rng, radiance) =
         estimate_direct rng wo i l scene.objs
       let light_pdf = 1 / f32.i32 (length scene.lights)
       in (rng, radiance / light_pdf)
