import "common"

type spectrum = { b0: (f32, f32)
                , b1: (f32, f32)
                , b2: (f32, f32)
                , b3: (f32, f32)
                , b4: (f32, f32)
                , b5: (f32, f32) }

let red_wavelen:   f32 = 610
let green_wavelen: f32 = 550
let blue_wavelen:  f32 = 460

let spectrum_to_arr (s: spectrum) : [6](f32, f32) =
  [ ((s.b0).0, (s.b0).1)
  , ((s.b1).0, (s.b1).1)
  , ((s.b2).0, (s.b2).1)
  , ((s.b3).0, (s.b3).1)
  , ((s.b4).0, (s.b4).1)
  , ((s.b5).0, (s.b5).1) ]

let spectrum_from_arr (xs: [6](f32, f32)): spectrum =
  { b0 = xs[0]
  , b1 = xs[1]
  , b2 = xs[2]
  , b3 = xs[3]
  , b4 = xs[4]
  , b5 = xs[5] }

let spectrum_lookup (v: f32) (s: spectrum) : f32 =
  -- TODO: SOACs don't seem to work well here. Compiler bug? Seems it
  --       can't compute size of memory to allocate before kernel
  --       start. Anywho, that's why we're not using `filter` etc.
  let ((w_below, x_below), (w_above, x_above)) =
    loop ((w_below, x_below), (w_above, x_above)) = ((-1, 0), (f32.inf, 0))
    for (w, x) in spectrum_to_arr s
    do if w > w_below && w <= v
       then ((w, x), (w_above, x_above))
       else if w < w_above && w > v
       then ((w_below, x_below), (w, x))
       else ((w_below, x_below), (w_above, x_above))
  in match (w_below < 0, f32.isinf w_above)
     case (true, true) -> 0
     case (true, false) -> x_above
     case (false, true) -> x_below
     case (false, false) ->
          f32.lerp x_below
                   x_above
                   ((v - w_below) / (w_above - w_below))

let map_spectrum (f: (f32, f32) -> (f32, f32)): spectrum -> spectrum =
  spectrum_from_arr <-< map f <-< spectrum_to_arr

let map_intensities (f: f32 -> f32): spectrum -> spectrum =
  map_spectrum (map_snd f)

let nm_to_m: f32 -> f32 = (* 1e-9)
let m_to_nm: f32 -> f32 = (* 1e9)

-- Computes the emitted radiance at the given temperature T in kelvin
-- according to Planck's Law.
--
-- PBR book 12.1.1
let blackbody (T: f32): spectrum =
  let c = 299792458
  let h = 6.62606957e-34
  let kb = 1.3806488e-23
  let ls = map nm_to_m [150, blue_wavelen, green_wavelen, red_wavelen, 1000, 2000]
  let planck l =
    (2 * h * c * c)
    / ((l**5) * (f32.exp ((h * c) / (l * kb  * T)) - 1))
  in spectrum_from_arr (map (\l -> (m_to_nm l, planck l)) ls)

let blackbody_normalized (T: f32): spectrum =
  let radiance = blackbody T
  let wiens_displacement = 2.8977721e-3
  let lambda_max = m_to_nm (wiens_displacement / T)
  let max_radiance = spectrum_lookup lambda_max radiance
  in map_intensities (/ max_radiance) radiance

let uniform_spectrum (intensity: f32): spectrum =
  { b0 = (0, intensity)
  , b1 = (-1, 0)
  , b2 = (-1, 0)
  , b3 = (-1, 0)
  , b4 = (-1, 0)
  , b5 = (-1, 0) }

let bright_blue_sky: spectrum = map_intensities (*5) (blackbody_normalized 17_000)
