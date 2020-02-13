import "shapes"

module type bvh = {
  type~ bvh_tree

  val build_bvh [n]: [n]geom -> bvh_tree

  val hit_bvh [m]: bounds -> ray -> [m]material -> bvh_tree -> maybe hit
}

module fake_bvh: bvh = {
  type~ bvh_tree = []geom

  let build_bvh = id

  let hit_bvh bn r mats xs =
    let select_min_hit a b =
      match (a, b)
      case (#nothing, _) -> b
      case (_, #nothing) -> a
      case (#just a', #just b') -> if a'.t < b'.t then a else b
    in reduce select_min_hit #nothing (map (hit_geom bn r mats) xs)
}

module dumb_bvh: bvh = {
  type node = #node { left: i32, bb: aabb, right: i32} | #leaf i32

  type~ bvh_tree = #empty | #nonempty { leaves: []geom, nodes: []node }

  let build_bvh xs =
    let n = length xs
    in if n == 0
       then #empty
       else let depth = f32.floor (f32.log2 n)
            let n_spills = n % (2 ** depth)
  -- TODO
  --
  -- Train of thought:
  -- At last (full) level: For each child at that level: Get "my" index by (i + 2) % 2^level.
  -- Compary my index to number of spills to know if I have 2 children or one.
  -- Compute child index by doing something like (my index - n_spills + n_spills * 2)

  let hit_bvh bn r mats xs = _
}
