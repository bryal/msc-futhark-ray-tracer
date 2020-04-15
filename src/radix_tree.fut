-- Tero Karras
-- "Maximizing Parallelism in the Construction of BVHs,Octrees, andk-d Trees"
--
-- Given a list of radix-sorted geometric primitives, efficiently
-- construct a radix tree that can later be converted to a BVH (or
-- some other acceleration structure) easily.

type ptr = #internal i32 | #leaf i32

local let div_rounding_up x y: i32 = (x + y - 1) / y

module radix_tree = {
  type node = { left: ptr, right: ptr, parent: i32 }

  -- `L` is a radix-sorted list of morton codes. The associated
  -- bounding-box/geometric primitive can be looked up later - we just
  -- have to remember the indices.
  --
  -- Computes the internal node array `I` and returns the tree
  -- structure.
  let mk [n] (L: [n]u32): []node =
    let delta (i: i32) (j: i32): i32 =
      if j >= 0 && j < n
      then let (Li, Lj) = unsafe (L[i], L[j])
           -- Handle duplicates by using index as tiebreaker
           in if Li == Lj
              then 32 + u32.clz (u32.i32 i ^ u32.i32 j)
              else u32.clz (Li ^ Lj)
      else -1

    let mk_node i =
      -- Determine direction of the range (+1 or -1)
      let d = i32.sgn (delta i (i + 1) - delta i (i - 1))
      -- Compute upper bound for the length of the range
      let delta_min = delta i (i - d)
      let l_max = loop l_max = 2
                  while delta i (i + l_max * d) > delta_min
                  do l_max * 2
      -- Find the other end using binary search
      let (l, _) = loop (l, t) = (0, l_max / 2)
                   while t >= 1
                   do let l = if delta i (i + (l + t) * d) > delta_min
                              then l + t
                              else l
                      in (l, t / 2)
      let j = i + l * d
      -- Find the split position using binary search
      let delta_node = delta i j
      let (s, _) = loop (s, q) = (0, 1)
                   while q <= l
                   do let t = div_rounding_up l (q * 2)
                      let s = if delta i (i + (s + t) * d) > delta_node
                              then s + t
                              else s
                      in (s, q * 2)
      let gamma = i + s * d + i32.min d 0
      -- Output child pointers
      let (left, left_internal_child) =
        if i32.min i j == gamma
        then (#leaf gamma, -1)
        else (#internal gamma, gamma)
      let (right, right_internal_child) =
        if i32.max i j == gamma + 1
        -- If the child of this node is not an internal node, we don't
        -- set it's parent. `scatter` will not do anything for
        -- negative indices.
        then (#leaf (gamma + 1), -1)
        else (#internal (gamma + 1), gamma + 1)
      in ( { left, right }
         , (left_internal_child, i)
         , (right_internal_child, i) )


    -- TODO: Can't this section be done any better? Feels suboptimal 🤔
    let n_nodes = n - 1
    let n_relations = n_nodes * 2
    let (I, left_relations, right_relations) =
      unzip3 (map mk_node (iota n_nodes))
    let (left_children, left_parents) = unzip left_relations
    let (right_children, right_parents) = unzip right_relations
    let children = left_children ++ right_children
    let parents = left_parents ++ right_parents
    let sorted_parents = scatter (replicate n_nodes (-1))
                                 (children :> [n_relations]i32)
                                 (parents :> [n_relations]i32)
    let I = map2 (\{ left, right } parent -> { left, right, parent })
                 I
                 sorted_parents
    in I
}
