(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2025                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* This HashTree implementation is from Frama-C, modified to be based on
   patricia-tree instead of the internal Intmap.

   The modifications are:
   - The type [t] is abstract.
   - [interf] is not implemented because no equivalent exist in PatriciaTree.
   - [diffq] is implemented as [difference]. We don't know if it should have
     been [symmetric_difference].
   - [subset] and [iterk] no longer accept two maps with different value types
     because PatriciaTree lacks a nonreflexive version of
     [reflexive_subset_domain_for_all2] and [fold_on_nonequal_inter].
*)

module type Key =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(K : Key) =
struct

  module Lmap = Listmap.Make(K)

  (* Using [SimpleNode] to avoid the overhead of constructing new values in
     [Node.view]. We'll use ints as the keys in the tree. *)
  module M = PatriciaTree.MakeMap (struct
      type t = int
      let to_int x = x
    end)

  type key = K.t

  type 'a t = 'a Lmap.t M.t (* sorted collisions *)

  let is_empty m =
    M.for_all (fun _key l -> l = []) m

  let empty = M.empty

  let _nonempty     = function [] -> None | l -> Some l
  let _nonempty_inv = function None -> [] | Some l -> l

  (* good sharing *)
  let insert f k v m =
    let h = K.hash k in
    M.insert h (function
        | Some old -> Lmap.insert f k v old
        | None -> [k, v])
      m

  let m_change f k x m =
    M.update k (f k x) m

  (* good sharing *)
  (*  val change : (key -> 'b -> 'a option -> 'a option) -> key -> 'b -> 'a t -> 'a t*)
  let change (f:key -> 'b -> 'a option -> 'a option) (k:key) (v:'b) (m:'a t) =
    let h = K.hash k in
    m_change (fun _h (k,v) -> function
        | None -> (match f k v None with | None -> None | Some w -> Some [k,w])
        | Some old -> _nonempty (Lmap.change f k v old)) h (k,v) m

  (* good sharing *)
  let add k v =
    insert (fun _k x _old -> x) k v

  let find k m = Lmap.find k (M.find (K.hash k) m)
  let findk k m = Lmap.findk k (M.find (K.hash k) m)

  let mem k m = try ignore (find k m) ; true with Not_found -> false

  let map f m = M.map_no_share (Lmap.map f) m

  (* TODO: index is not increasing. *)
  let mapi f m = M.map_no_share (Lmap.mapi f) m

  let mapf f = M.filter_map_no_share (fun _h w -> _nonempty (Lmap.mapf f w))

  (* good sharing *)
  let mapq f = M.filter_map (fun _h w -> _nonempty (Lmap.mapq f w))

  (* good sharing *)
  let filter f = M.filter_map (fun _k w -> _nonempty (Lmap.filter f w))

  (* good sharing *)
  let remove k m =
    let h = K.hash k in
    m_change (fun _h k ->  function
        | None -> None
        | Some old -> _nonempty (Lmap.remove k old)) h k m

  let iter f m = M.iter (fun _k l -> Lmap.iter f l) m

  (* Inefficiently implemented: Each sublists are merged with the ever growing
     accumulator. O(n^2) time. *)
  let to_sorted_list m =
    M.fold
      (fun _k l acc -> List.merge (fun a b -> K.compare (fst a) (fst b)) l acc)
      m []

  let iter_sorted f m =
    List.iter (fun (k, v) -> f k v) (to_sorted_list m)

  let fold f m a = M.fold (fun _k l acc -> Lmap.fold f l acc) m a
  let fold_sorted f m a =
    List.fold_left (fun acc (k,v) -> f k v acc) a (to_sorted_list m)

  let size m = fold (fun _ _ w -> succ w) m 0

  (* good sharing *)
  let partition p m =
    M.fold (fun k v (left, right) ->
        let l, r = Lmap.partition p v in
        M.add k l left, M.add k r right
      ) m (empty, empty)

  (* good sharing *)
  let union f = M.idempotent_union (fun _h -> Lmap.union f)

  let inter f = M.nonidempotent_inter_no_share (fun _h -> Lmap.inter f)

  (* good sharing *)
  let interq f =
    M.idempotent_inter_filter (fun _h a b -> _nonempty (Lmap.interq f a b))

  (* good sharing *)
  let diffq f = M.difference (fun _h a b -> _nonempty (Lmap.diffq f a b))

  let subset f a b =
    let f _k a b = Lmap.subset f a b in
    M.reflexive_subset_domain_for_all2 f a b

  let equal eq m1 m2 =
    M.reflexive_same_domain_for_all2 (fun _k a b -> Lmap.equal eq a b) m1 m2

  let iterk f a b =
    M.fold_on_nonequal_inter (fun _k a b () -> Lmap.iterk f a b) a b ()

  (* good sharing *)
  let merge f = M.slow_merge (fun _h u1 u2 ->
      _nonempty (Lmap.merge f (_nonempty_inv u1) (_nonempty_inv u2)))

end

