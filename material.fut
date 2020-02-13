-- All *_sample_dir functions return `wi` vectors in world space

-- TODO: All kinds of materials

import "common"

type dir_sample = { wi: vec3, bsdf: vec3, pdf: f32 }

-- Sample a point on a hemisphere with a cosine-weighted distribution.
--
-- Works by first sampling uniformly on a disk, and then projecting
-- the point up onto the hemisphere. This is called Malley's Method.
-- See PBR Book 13.6.3.
--
-- Θ is the angle between the up-axis and the sampled direction. sin Θ
-- is the radius, and cos Θ is thus the height of the projected
-- sample.
--
-- Range of Θ is exclusive, i.e. angle between z and plane is <
-- 90deg. No point in sampling directions that will have a PDF-value
-- of zero!
let cosine_sample_hemisphere (rng: rnge): (rnge, vec3) =
  let (rng, d) = random_in_unit_disk rng
  let sin2theta = d.x * d.x + d.y * d.y
  -- Float error may cause negativity?
  let cos2theta = f32.max 0 (1 - sin2theta)
  let z = f32.sqrt cos2theta
  in (rng, mkvec3 d.x d.y z)

-- Construct an arbitrary orthonormal base for a normal, transform a
-- vector into it
let orthonormal_basis_inverse_transform (normal: vec3) (w: vec3)
                                      : vec3 =
  let binormal = if f32.abs normal.x > f32.abs normal.z
                 then vec3.normalise (mkvec3 (-normal.y) normal.x 0)
                 else vec3.normalise (mkvec3 0 (-normal.z) normal.y)
  let tangent = vec3.cross binormal normal
  in vec3.scale w.x tangent
     vec3.+ vec3.scale w.y binormal
     vec3.+ vec3.scale w.z normal

let diffuse_bsdf (color: vec3): vec3 =
  vec3.scale inv_pi color

-- TODO: Consider Oren-Nayar model instead of Lambertian
--
-- Diffuse reflection according to Lambertian model
let diffuse_sample_dir (h: hit) (rng: rnge): dir_sample =
  let (_, dir) = cosine_sample_hemisphere rng
  let cosTheta = dir.z
  let wi = orthonormal_basis_inverse_transform h.normal dir
  -- TODO: Can / when do we get wi in the wrong hemisphere? Float
  --       error? Consider returning pdf and/or bsdf = 0 when that is
  --       the case.
  in { wi, bsdf = diffuse_bsdf h.mat.color, pdf = cosTheta * inv_pi }

let refraction_sample_dir (_wo: vec3) (_h: hit) (_rng: rnge)
                        : { wi: vec3, bsdf: vec3, pdf: f32 } =
  { wi = mkvec3 0 0 0, bsdf = mkvec3 0 0 0, pdf = 0 }

let dielectric_refraction_sample_dir (wo: vec3) (h: hit) (rng: rnge)
                                   : dir_sample =
  -- TODO: Transmission or diffuse reflection
  if false -- some opacity parameter of the material?
  then refraction_sample_dir wo h rng
  else diffuse_sample_dir h rng


let dielectric_reflection_sample_dir (_wo: vec3) (_h: hit) (_rng: rnge)
                                   : dir_sample =
  -- TODO
  { wi = mkvec3 0 0 0, bsdf = mkvec3 0 0 0, pdf = 0 }

let dielectric_sample_dir (wo: vec3) (h: hit) (rng: rnge)
                        : dir_sample =
  -- TODO: Russian roulette with Fresnel to decide whether to reflect
  --       or refract
  if false -- h.mat.ref_ix
  then dielectric_reflection_sample_dir wo h rng
  else dielectric_refraction_sample_dir wo h rng

-- TODO
let metal_sample_dir (_wo: vec3) (_h: hit) (_rng: rnge)
                   : dir_sample =
  { wi = mkvec3 0 0 0, bsdf = mkvec3 0 0 0, pdf = 0 }

-- Sample a direction accodring to a distribution that is similar to
-- the material's corresponding BSDF distribution.
let uber_sample_dir (wo: vec3) (h: hit) (rng: rnge)
                  : dir_sample =
  -- TODO: Russian roulette to decide whether to sample metal or dielectric
  if false -- h.mat.metalness
  then metal_sample_dir wo h rng
  else dielectric_sample_dir wo h rng
