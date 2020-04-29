let map_fst 'a 'b 'c (f: a -> c) (x: a, y: b): (c, b) = (f x, y)
let map_snd 'a 'b 'c (f: b -> c) (x: a, y: b): (a, c) = (x, f y)

module maybe = {
  type maybe 't = #nothing | #just t

  let is_just 'a (x: maybe a): bool =
    match x case #just _ -> true
            case #nothing -> false

  let map 'a 'b (f: a -> b) (x: maybe a): maybe b =
    match x
    case #just a -> #just (f a)
    case #nothing ->  #nothing

  let when 'a (pred: bool) (x: maybe a): maybe a =
    if pred then x else #nothing

  let guard 'a (pred: bool) (x: a): maybe a =
    if pred then #just x else #nothing

  let or 'a (x: maybe a) (y: maybe a): maybe a =
    match x
    case #just a -> #just a
    case #nothing -> y

  let unwrap_or 'a (default: a) (x: maybe a): a =
    match x
    case #just a -> a
    case #nothing -> default
}

type maybe 't = maybe.maybe t

let approx_zero (a: f32) (eps: f32): bool = a > -eps && a < eps

let clamp ((min, max): (f32, f32)) (x: f32): f32 =
  f32.max min (f32.min max x)

let minimum_by 'a (f: a -> f32) (xs: []a): maybe a =
  if null xs
  then #nothing
  else #just (let y = head xs
              in (.0)
              <| loop (y, yv) = (y, f y)
                 for x in xs
                 do let xv = f x
                    in if xv < yv
                       then (x, xv)
                       else (y, yv))
