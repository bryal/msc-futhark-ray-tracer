import "common"

-- TODO: Glossy reflections. Check out wikipedia/Schlick's
-- approximation. Use halfway vector? Rest of the microfacet model.
let schlick (wi: vec3) (normal: vec3) (n1: f32) (n2: f32): f32 =
  let r0 = let x = (n1 - n2) / (n1 + n2) in x * x
  in r0 + (1 - r0) * (1 + vec3.dot normal wi)**5

let reflect (wi: vec3) (n: vec3): vec3 =
  wi vec3.- vec3.scale (2 * vec3.dot wi n) n

-- Returns #nothing on total internal reflection
--
-- We use the (apparently, we have not benchmarked it) efficient
-- method of Bec presented in Real Time Rendering formula 14.28
-- (p. 627) to compute the refraction vector. This is an
-- implementation of Snell's Law, not an approximation.
--
-- About `discriminant`: "Total internal reflection is indicated by a
-- negative radiccand in the equation for cos(θ₂)" according to
-- wikipedia/Snell's Law. k = cos(θ₂) in our function.
let refract (wi: vec3) (n: vec3) (relative_ix: f32): maybe vec3 =
  let l = vec3.scale (-1) wi
  let w = relative_ix * vec3.dot l n
  let discriminant = 1 + (w - relative_ix) * (w + relative_ix)
  in if discriminant < 0
     then #nothing
     else let k = f32.sqrt discriminant
          let t = vec3.scale (w - k) n vec3.- vec3.scale relative_ix l
          in #just t

-- Transmittance: The amount of light not absorbed by the hit object
--
-- NOTE: What we call transmittance here is called attenuation in
-- RTi1W, but that seemed inversed to us.
--
-- NOTE: In some literature, wi and wo are reversed, so wi is the
-- direction for the incoming light from the sun, and wo is the
-- outgoing vector towards the camera
let scatter (wi: vec3) (h: hit) (rng: rnge)
          : { transmit: vec3, wo: vec3 } =
  -- TODO: Do a linearblend between metal and dielectric
  if h.mat.metalness > 0 then -- is metal
    let reflected = reflect wi h.normal
    let scatter_sphere =
      vec3.scale h.mat.fuzz (random_in_unit_sphere rng)
    let wo = vec3.normalise (reflected vec3.+ scatter_sphere)
    -- If we hit a fuzzy metal sphere close to the edge, i.e. tangent
    -- / almost tangent it, the scatter sphere we "create" may
    -- intersect the metal sphere, and we may sample a wo that is
    -- actually pointing inwards. We're not sure what the real-life
    -- equivalence is, but as the RTi1W did, we simply return black in
    -- those cases, treating it as if the sphere absorbed the ray.
    let transmit =
      if vec3.dot wo h.normal > 0 then h.mat.color else mkvec3 0 0 0
    in { transmit, wo }
  -- TODO: Make dielectric and diffuse mats hierarchical, so we
  -- compute the reflectio n first, but if the refraction index is 1,
  -- we will refract to the underlying diffuse material. Also handle
  -- underlying transmitting material.
  else if h.mat.ref_ix <= 1 then -- is diffuse
    let target = h.pos
                 vec3.+ h.normal
                 vec3.+ random_in_unit_sphere rng
    let wo = vec3.normalise (target vec3.- h.pos)
    in { transmit = h.mat.color, wo }
  else -- is dielectric
    let (outward_normal, n1, n2) =
      if vec3.dot wi h.normal > 0
      then (vec3.scale (-1) h.normal, h.mat.ref_ix, 1.0)
      else (h.normal, 1.0, h.mat.ref_ix)
    let relative_ix = n1 / n2
    let (_, russian_roulette) = dist.rand (0,1) rng
    -- TODO: Do we really need a nothing case here? Seems like schlick
    -- could immediately tell us if there's total internal reflection.
    let wo =
      match refract wi outward_normal relative_ix
      case #just refracted ->
        if russian_roulette >= schlick wi outward_normal n1 n2
        then refracted
        else reflect wi h.normal
      case #nothing -> reflect wi h.normal
    in { transmit = mkvec3 1 1 1, wo }
