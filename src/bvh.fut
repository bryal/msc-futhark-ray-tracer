import "../lib/github.com/diku-dk/sorts/radix_sort"

import "radix_tree"
import "shapes"

module type Geom = {
  type t
  val get_geom: t -> geom
}

module type bvh = {
  type t
  type~ bvh

  val build [n]: [n]t -> bvh

  -- For recursive ray-tracing / indirect illumination
  val closest_hit: f32 -> ray -> bvh -> maybe (t, hit)

  -- For shadow rays / direct illumination. When only care about
  -- whether we hit *anything* before reaching the light source.
  val any_hit: f32 -> ray -> bvh -> bool
}

module mk_fake_bvh (G: Geom): bvh with t = G.t = {
  type t = G.t
  type~ bvh = []G.t

  let build = id

  let closest_hit tmax r xs: maybe (G.t, hit) =
    let select_min_hit a b: maybe (G.t, hit) =
      match (a, b)
      case (#nothing, _) -> b
      case (_, #nothing) -> a
      case (#just (_, ah), #just (_, bh)) ->
        if ah.t < bh.t then a else b
    let hit x = maybe.map (\h -> (x, h)) (hit_geom tmax r (G.get_geom x))
    in reduce select_min_hit #nothing (map hit xs)

  let any_hit tmax r xs =
    any (maybe.is_just <-< hit_geom tmax r <-< G.get_geom) xs
}

let morton_n_bits: u32 = 30
let morton_component_n_bits: u32 = morton_n_bits / 3
let morton_component_max_val: f32 =
  f32.u32 (2**morton_component_n_bits - 1)

-- Expands a 10-bit integer into 30 bits by inserting 2 zeros after
-- each bit.
let expand_bits (x: u32): u32 =
    let x = (x * 0x00010001) & 0xFF0000FF
    let x = (x * 0x00000101) & 0x0F00F00F
    let x = (x * 0x00000011) & 0xC30C30C3
    let x = (x * 0x00000005) & 0x49249249
    in x

-- Calculates a 30-bit Morton code for the given 3D point located
-- within the unit cube [0,1].
--
-- Morton code: (X0X1X2..., Y0Y1Y2..., Z0Z1Z2...)
--           => X0Y0Z0X1Y1Z1X2Y2Z2...
--
-- Uses Karras's method from
-- "Thinking Parallel, Part III: Tree Construction on the GPU"
let morton3D (v: vec3): u32 =
  let { x, y, z } = vmin (vec3.scale (morton_component_max_val + 1) v)
                         (mkvec3_repeat morton_component_max_val)
  let (xx, yy, zz) = ( expand_bits (u32.f32 x)
                     , expand_bits (u32.f32 y)
                     , expand_bits (u32.f32 z) )
  in xx * 4 + yy * 2 + zz

module mk_lbvh (G: Geom): bvh with t = G.t = {
  type node = { aabb: aabb, left: ptr, right: ptr, parent: i32 }

  type t = G.t
  type~ bvh =
    -- The tree has its own unit-cube space. Everything must be
    -- transformed into it.
    { bounds: aabb
    , leaves: []t
    , nodes: []node }

  let build [n] (xs: [n]t): bvh =
    let aabbs = map (bounding_box_geom <-< G.get_geom) xs
    let neutral_aabb = { center = mkvec3 0 0 0
                       , half_dims = mkvec3_repeat (-f32.inf) }
    let bounds = reduce_comm containing_aabb neutral_aabb aabbs
    let normalise_position p =
      (p vec3.- aabb_min_corner bounds) vec3./ aabb_dimensions bounds
    let mortons = map (morton3D <-< normalise_position <-< (.center))
                      aabbs
    let (xs, aabbs, mortons) =
      unzip3 (radix_sort_by_key (.2) u32.num_bits u32.get_bit
                                (zip3 xs aabbs mortons))
    let I = radix_tree.mk mortons

    -- TODO: This is so wasteful! Is there really no better way of
    -- doing it? Is it even that much faster than doing it
    -- sequentially, considering how many wasted computations there
    -- are. Consider the case where the number of internal nodes is
    -- way to big for the GPU to handle in one "round". Bench this.
    let initial_empty_aabb { left, right, parent } =
      { aabb = { center = mkvec3 0 0 0, half_dims = mkvec3 0 0 0 }
      , left, right, parent }
    let I = map initial_empty_aabb I
    let depth = i32.f32 (f32.log2 (f32.i32 n)) + 2
    let get_aabb inners ptr =
      match ptr
      case #leaf i -> unsafe aabbs[i]
      case #internal i -> unsafe inners[i].aabb
    let update inners { aabb = _, left, right, parent} =
      { aabb = containing_aabb (get_aabb inners left)
                               (get_aabb inners right)
      , left, right, parent }
    let I = loop I
            for _i < depth
            do map (update I) I
    in { bounds, leaves = xs, nodes = I }

  let closest_hit (tmax: f32) (r: ray) (bvh: bvh)
                : maybe (t, hit) =
    let (closest, _, _, _) =
      loop (closest, tmax, current, prev) = (-1, tmax, 0, #internal (-1))
      while current != -1
      do let node = unsafe bvh.nodes[current]
         let rec_child = maybe.or
           (maybe.guard (prev == node.left)
                        node.right)
           (maybe.guard (prev != node.right && hit_aabb tmax r node.aabb)
                        node.left)
         in match rec_child
            case #nothing -> (closest, tmax, node.parent, #internal current)
            case #just ptr ->
              match ptr
              case #internal i -> (closest, tmax, i, #internal current)
              case #leaf i ->
                match hit_geom tmax r (G.get_geom (unsafe bvh.leaves[i]))
                case #just hit -> (i, hit.t, current, ptr)
                case #nothing -> (closest, tmax, current, ptr)
    in maybe.when (closest >= 0)
       <| let a = unsafe bvh.leaves[closest]
          in maybe.map (\h -> (a, h)) (hit_geom tmax r (G.get_geom a))

  -- TODO: Can probably be made faster. Just a basic improved version
  --       of closest_hit atm.
  let any_hit (tmax: f32) (r: ray) (bvh: bvh): bool =
    (.0) <|
      loop (hit, current, prev) = (false, 0, #internal (-1))
      while !hit && current != -1
      do let node = unsafe bvh.nodes[current]
         let rec_child = maybe.or
           (maybe.guard (prev == node.left)
                        node.right)
           (maybe.guard (prev != node.right && hit_aabb tmax r node.aabb)
                        node.left)
         in match rec_child
            case #nothing -> (false, node.parent, #internal current)
            case #just ptr ->
              match ptr
              case #internal i -> (false, i, #internal current)
              case #leaf i ->
                match hit_geom tmax r (G.get_geom (unsafe bvh.leaves[i]))
                case #just _ -> (true, current, ptr)
                case #nothing -> (false, current, ptr)
}
