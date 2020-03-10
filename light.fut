import "common"
import "shapes"
import "material"

-- let light_pdf [n] (lights: [n]geom): f32 = 1 / f32.i32 n

-- let sample_light [n] (wo: vec3) (h: hit) (lights: [n]geom) (rng: rnge)
--                    : (rnge, dir_sample) =
--   let (rng, x) = random_unit_exclusive rng
--   let i = i32.f32 (x * f32.i32 n)
--   let p = 1 / f32.i32 n
--   let l = unsafe lights[i]
--   -- TODO: Sample whole triangle, and don't base on inaccurate AABB
--   let point = (bounding_box_geom l).center
--   let v = point vec3.- h.pos
--   let wi = vec3.normalise v
--   let falloff = f32.abs (vec3.dot (mkvec3 0 (-1) 0) (vec3_neg wi)) / (vec3.norm v)**2
--   in (rng, { wi, bsdf = -- vec3.scale falloff
--                                    (bsdf wo wi h), pdf = p })
