-- All sampling functions except `sample_dir` work in local space
-- around the axis vectors. `world_to_local` transforms the ray into
-- the local space, and `local_to_world` transform the vectors back
-- out into world space. The reason for this is that many calculations
-- are simpler and cheaper when the normal is the z-axis (0, 0, 1).

import "common"

let null_sample: dir_sample =
  { wi = mkvec3 0 0 0, bsdf = mkvec3 0 0 0, pdf = 0 }

let local_normal = mkvec3 0 0 1

-- In local space
-- **************

-- TODO: Bench these, and see if it makes much of a difference if they
--       are less inlined.

let cos_theta  (w: vec3): f32 = w.z
let cos2_theta (w: vec3): f32 = w.z * w.z
let sin2_theta (w: vec3): f32 = f32.max 0 (1 - cos2_theta w)
let sin_theta  (w: vec3): f32 = f32.sqrt (sin2_theta w)
let tan_theta  (w: vec3): f32 = sin_theta w / cos_theta w
let tan2_theta (w: vec3): f32 = sin2_theta w / cos2_theta w

let cos_phi w =
  let st = sin_theta w
  in if st == 0 then 1 else clamp (-1, 1) (w.x / st)
let sin_phi w =
  let st = sin_theta w
  in if st == 0 then 0 else clamp (-1, 1) (w.y / st)
let cos2_phi w = cos_phi w * cos_phi w
let sin2_phi w = sin_phi w * sin_phi w

let same_hemisphere (w: vec3) (u: vec3): bool =
  w.z * u.z > 0

-- **************

let reflect (w: vec3) (n: vec3): vec3 =
  vec3.scale (-1) w vec3.+ vec3.scale (2 * vec3.dot w n) n

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

let diffuse_bsdf (color: vec3): vec3 =
  vec3.scale inv_pi color

let diffuse_pdf (wo: vec3) (wi: vec3): f32 =
  if same_hemisphere wo wi
  then cos_theta wi * inv_pi
  else 0

-- TODO: Consider Oren-Nayar model instead of Lambertian
--
-- Diffuse reflection according to Lambertian model
let diffuse_sample_dir (m: material) (rng: rnge): (rnge, dir_sample) =
  let (rng, wi) = cosine_sample_hemisphere rng
  -- TODO: Can / when do we get wi in the wrong hemisphere? Float
  --       error? Consider returning pdf and/or bsdf = 0 when that is
  --       the case.
  in (rng, { wi, bsdf = diffuse_bsdf m.color, pdf = cos_theta wi * inv_pi })

-- PBR Book 8.2.3
let refract (wi: vec3) (n: vec3) (eta: f32)
          : #refraction vec3 | #total_internal_reflection vec3 =
  let cos_theta_i = vec3.dot n wi
  let sin2_theta_i = f32.max 0 (1 - cos_theta_i * cos_theta_i)
  let sin2_theta_t = eta * eta * sin2_theta_i
  in if sin2_theta_t >= 1
     then #total_internal_reflection (reflect wi n)
     else let cos_theta_t = f32.sqrt (1 - sin2_theta_t)
          let wt = vec3.scale (-eta) wi
                   vec3.+ vec3.scale (eta * cos_theta_i - cos_theta_t) n
          in #refraction wt

-- For perfectly specular reflection/refraction, PBRT defines `f()` to
-- be constantly `0` since they handle reflection functions with
-- singularities specially in the transport routines. Not sure what
-- that means exactly, and if we also do that already. We'll go along
-- and just always return 0 for now.
--
-- PBR Book 8.2.2:
--    For an arbitrary pair of directions the delta function returns
--    no scattering. If the caller happened to pass a vector and its
--    perfect mirror direction, this function still returns zero.
--    Although this might be a slightly confusing interface to these
--    reflection functions, we still get the correct result in the end
--    because reflection functions involving singularities with delta
--    distributions receive special handling by the light transport
--    routines (see Chapter 14).
let transmission_bsdf: vec3 = mkvec3 0 0 0

-- PBR Book 14.1.3
let transmission_pdf: f32 = 0

-- Either refract into / out of a transmitting material, or total
-- internal reflection. PBR Book 8.2.3
let transmission_sample_dir (wo: vec3) (m: material)
                          : dir_sample =
  let entering = cos_theta wo > 0
  let eta_air = 1.0
  let (n, eta) = if entering
                 then (local_normal, eta_air / m.ref_ix)
                 else (vec3_neg local_normal, m.ref_ix / eta_air)
  in match refract wo n eta
     case #refraction wi ->
       { wi
       , bsdf = mkvec3_repeat (1 / f32.abs (cos_theta wi))
       , pdf = 1 }
     case #total_internal_reflection wi ->
       { wi
       -- TODO: This BSDF may well be wrong. Fix somehow. It's up to
       --       you, future us!
       , bsdf = mkvec3_repeat (1 / f32.abs (cos_theta wi))
       , pdf = 1 }

-- Note that attenuation by fresnel reflectance does not happen here,
-- but is handled in `dielectric_bsdf` instead.
let dielectric_refraction_bsdf (m: material): vec3 =
  vec3_lerp transmission_bsdf (diffuse_bsdf m.color) m.opacity

let dielectric_refraction_pdf (wo: vec3) (wh: vec3) (m: material): f32 =
  f32.lerp transmission_pdf
           (diffuse_pdf wo wh)
           m.opacity

let dielectric_refraction_sample_dir (wo: vec3) (m: material) (rng: rnge)
                                   : (rnge, dir_sample) =
  let (rng, p) = random_unit_exclusive rng
  in if p < m.opacity
     then diffuse_sample_dir m rng
     else (rng, transmission_sample_dir wo m)

-- TODO: Handle when we come from the inside, exiting. Like through
--       glass.
--
-- Fresnel reflectance, called F in literature, via Schlick's
-- approximation.
let fresnel_reflectance (wo: vec3) (m: material): f32 =
  let ix_1 = let air = 1 in air
  let ix_2 = m.ref_ix
  let r0 = let x = (ix_1 - ix_2) / (ix_1 + ix_2) in x * x
  in r0 + (1 - r0) * ((1 - cos_theta wo) ** 5)

-- Called D in literature. For use with the Torrance-Sparrow
-- microfacet model.
--
-- Traditional Beckmann-Spizzichino model, based on the
-- BeckmannDistribution in PBR Book 8.4.2.
let microfacet_distribution (alpha: f32) (wh: vec3): f32 =
  let tan2_theta = tan2_theta wh
  in if f32.isinf tan2_theta
     then 0
     else f32.exp (-tan2_theta / (alpha * alpha))
          / (f32.pi * alpha * alpha * cos2_theta wh * cos2_theta wh)

-- Called G in literature. For use with the Torrance-Sparrow
-- microfaced model.
--
-- Beckmann-Spizzichino
let self_shadowing_factor (alpha: f32) (wo: vec3) (wi: vec3): f32 =
  -- Approximation that avoids the expensive functions `erf` and `exp`
  let lambda w =
    let abs_tan_theta = f32.abs (tan_theta w)
    in if f32.isinf abs_tan_theta then 0 else
    let a = 1 / (alpha * abs_tan_theta)
    in if a >= 1.6
       then 0
       else (1 - 1.259 * a + 0.396 * a * a)
            / (3.535 * a + 2.181 * a * a)
  in 1 / (1 + lambda wo + lambda wi)

let beckmann_alpha (roughness: f32): f32 =
  -- TODO: Investigate why the curve from the book becomes fuzzy so
  --       fast! Understand this alpha value and the
  --       Taylor-approximation properly.
  --
  -- Linear roughness:
  --
  let eps = 0.004
  let roughness = f32.max eps roughness
  in 1.62142 * roughness
  --
  -- Nonlinear roughness:
  --
  -- let eps = 0.001
  -- let roughness = f32.max eps roughness
  -- let x = f32.log roughness
  -- in 1.62142
  --    + 0.819955 * x
  --    + 0.1734 * x * x
  --    + 0.0171201 * x * x * x
  --    + 0.000640711 * x * x * x * x

let microfacet_distribution_pdf (wh: vec3) (m: material): f32 =
  let alpha = beckmann_alpha m.roughness
  in microfacet_distribution alpha wh * f32.abs (cos_theta wh)

-- D * G in literature. Implementation of the Beckmann–Spizzichino
-- model, based on PBR Book 8.4.2 & 8.4.3
let microfacet_factor (wo: vec3) (wi: vec3) (m: material): f32 =
  let wh = vec3.normalise (wi vec3.+ wo)
  let alpha = beckmann_alpha m.roughness
  in microfacet_distribution alpha wh
     * self_shadowing_factor alpha wo wi

-- Torrance-Sparrow microfacet model
--
-- Note that the F (fresnel reflectance) factor is not included
-- here. Instead, we sample reflection vs. refraction in
-- `dielectric_sample_dir` with a frequency of F, and leave the
-- PDF-value unchanged. That accomplishes the same thing.
let dielectric_reflection_bsdf (wo: vec3) (wi: vec3) (m: material): vec3 =
  mkvec3_repeat <|
    microfacet_factor wo wi m
    / (4 * cos_theta wo * cos_theta wi)

-- Spherical angles to direction vector
let spherical_direction (sin_theta: f32) (cos_theta: f32) (phi: f32): vec3 =
  mkvec3 (sin_theta * f32.cos phi)
         (sin_theta * f32.sin phi)
         cos_theta

-- Sample a halfway-vector from the visible Beckmann-Spizzichino
-- distribution.
--
-- Note that we're only doing an isotropic distribution, so alphax ==
-- alphay, which implies that alpha = alphax = alphay.
let dielectric_reflection_sample_wh (wo: vec3) (m: material) (rng: rnge)
                                  : (rnge, vec3, f32) =
  let (rng, (u0, u1)) = random_in_unit_square rng
  let log_sample = f32.log (1 - u0)
  in if f32.isinf log_sample then (rng, mkvec3 0 0 0, 0) else
  let alpha = beckmann_alpha m.roughness
  let tan2_theta = -alpha * alpha * log_sample
  let phi = u1 * 2 * f32.pi
  let cos_theta = 1 / f32.sqrt (1 + tan2_theta)
  let sin_theta = f32.sqrt (f32.max 0 (1 - cos_theta * cos_theta))
  let wh = spherical_direction sin_theta cos_theta phi
  let wh = if same_hemisphere wo wh then wh else (vec3_neg wh)
  let pdf_wh = microfacet_distribution alpha wh * f32.abs cos_theta
  in (rng, wh, pdf_wh)

let dielectric_reflection_pdf (wo: vec3) (wi: vec3) (m: material): f32 =
  if !(same_hemisphere wo wi)
  then 0
  else let wh = vec3.normalise (wo vec3.+ wi)
       in microfacet_distribution_pdf wh m / (4 * vec3.dot wo wh)

-- PBR Book 14.1.1
let dielectric_reflection_sample_dir (wo: vec3) (m: material) (rng: rnge)
                                   : (rnge, dir_sample) =
  let (rng, wh, pdf_wh) = dielectric_reflection_sample_wh wo m rng
  let wi = reflect wo wh
  in if !(same_hemisphere wo wi)
     then (rng, null_sample)
     else ( rng
          , { wi
            , bsdf = dielectric_reflection_bsdf wo wi m
            , pdf = pdf_wh / (4 * vec3.dot wo wh) } )

let dielectric_bsdf (wo: vec3) (wi: vec3) (m: material): vec3 =
  let reflectance = if cos_theta wo <= 0
                    then 0
                    else fresnel_reflectance wo m
  in vec3_lerp (dielectric_refraction_bsdf m)
               (dielectric_reflection_bsdf wo wi m)
               reflectance

let dielectric_pdf (wo: vec3) (wi: vec3) (m: material): f32 =
  if cos_theta wo <= 0
  then dielectric_refraction_pdf wo wi m
  else f32.lerp (dielectric_refraction_pdf wo wi m)
                (dielectric_reflection_pdf wo wi m)
                (fresnel_reflectance wo m)

-- NOTE: May not respect conservation of energy properly, as we're
--       just adapting Torrance-Sparrow to a fresnel-blend material
--       intuitively. PBR book 8.5 talks more about this. Look at the
--       Ashikhmin and Shirley model.
let dielectric_sample_dir (wo: vec3) (m: material) (rng: rnge)
                        : (rnge, dir_sample) =
  if cos_theta wo <= 0 -- Coming from the inside
  then dielectric_refraction_sample_dir wo m rng
  else let r = fresnel_reflectance wo m
       let (rng, p) = random_unit_exclusive rng
       in if p < r
          then dielectric_reflection_sample_dir wo m rng
          else dielectric_refraction_sample_dir wo m rng

let metal_bsdf (wo: vec3) (wi: vec3) (m: material): vec3 =
  m.color vec3.* dielectric_reflection_bsdf wo wi m

let metal_pdf (wo: vec3) (wi: vec3) (m: material): f32 =
  dielectric_reflection_pdf wo wi m

let metal_sample_dir (wo: vec3) (m: material) (rng: rnge)
                   : (rnge, dir_sample) =
  let (rng, sample) = dielectric_reflection_sample_dir wo m rng
  in (rng, sample with bsdf = m.color vec3.* sample.bsdf)

let uber_bsdf (wo: vec3) (wi: vec3) (m: material): vec3 =
  vec3_lerp (dielectric_bsdf wo wi m) (metal_bsdf wo wi m) m.metalness

let uber_pdf (wo: vec3) (wi: vec3) (m: material): f32 =
  f32.lerp (metal_pdf wo wi m) (dielectric_pdf wo wi m) m.metalness

-- Sample a direction accodring to a distribution that is similar to
-- the material's corresponding BSDF distribution.
let uber_sample_dir (wo: vec3) (m: material) (rng: rnge)
                  : (rnge, dir_sample) =
  let (rng, p) = random_unit_exclusive rng
  in if p < m.metalness
     then metal_sample_dir wo m rng
     else dielectric_sample_dir wo m rng

type orthonormal_basis = { tangent: vec3, binormal: vec3, normal: vec3 }

let mk_orthonormal_basis (normal: vec3): orthonormal_basis =
  let binormal = if f32.abs normal.x > f32.abs normal.z
                 then vec3.normalise (mkvec3 (-normal.y) normal.x 0)
                 else vec3.normalise (mkvec3 0 (-normal.z) normal.y)
  let tangent = vec3.cross binormal normal
  in { tangent, binormal, normal }

let world_to_local (onb: orthonormal_basis) (w: vec3): vec3 =
  mkvec3 (vec3.dot w onb.tangent)
         (vec3.dot w onb.binormal)
         (vec3.dot w onb.normal)

-- Construct an arbitrary orthonormal base for a normal, transform a
-- vector into it
let local_to_world (onb: orthonormal_basis) (w: vec3): vec3 =
  vec3.scale w.x onb.tangent
  vec3.+ vec3.scale w.y onb.binormal
  vec3.+ vec3.scale w.z onb.normal

let bsdf_f (wo: vec3) (wi: vec3) (h: hit): vec3 =
  let onb = mk_orthonormal_basis h.normal
  let (wo, wi) = (world_to_local onb wo, world_to_local onb wi)
  in uber_bsdf wo wi h.mat

let bsdf_pdf (wo: vec3) (wi: vec3) (h: hit): f32 =
  let onb = mk_orthonormal_basis h.normal
  let (wo, wi) = (world_to_local onb wo, world_to_local onb wi)
  in uber_pdf wo wi h.mat

-- To make some calculations simpler, compute all
-- reflection/refraction vectors in a local space where the normal is
-- simply (0, 0, 1).
let sample_dir (wo: vec3) (h: hit) (rng: rnge): (rnge, dir_sample) =
  let onb = mk_orthonormal_basis h.normal
  let wo' = world_to_local onb wo
  let (rng, s) = uber_sample_dir wo' h.mat rng
  in (rng, s with wi = local_to_world onb s.wi)
