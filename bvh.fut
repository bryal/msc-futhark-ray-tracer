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
