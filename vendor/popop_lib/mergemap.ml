(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2024                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Merging Map Functor                                                --- *)
(* -------------------------------------------------------------------------- *)
open Base

module Int = struct
  type t = int

  let equal (x : int) y = x = y
  let tag x = x
  let pp = Pp.int
end

module GM = Intmap.Make (Int)
module Intmap = GM.NT

module Make
    (K : Map_intf.OrderedHashedType)
(* :
   Map_intf.Map with type key = K.t and type 'a data = 'a*) =
struct
  type 'a data = 'a

  module Lmap = Listmap.Make (K)

  type key = K.t
  type 'a t = 'a Lmap.t Intmap.t (* sorted collisions *) [@@deriving hash]

  let equal f m1 m2 = Intmap.equal (Lmap.equal f) m1 m2
  let compare f m1 m2 = Intmap.compare (Lmap.compare f) m1 m2
  let is_empty m = Intmap.for_all (fun _ m -> List.is_empty m) m
  let singleton k v = Intmap.singleton (K.hash k) (Lmap.singleton k v)
  let empty = Intmap.empty
  let _nonempty = function [] -> None | l -> Some l
  let _nonempty_inv = function None -> [] | Some l -> l
  let check_invariant x = Intmap.check_invariant x

  (* good sharing *)
  let change (f : 'a option -> 'a option) (k : key) (m : 'a t) =
    let h = K.hash k in
    Intmap.change
      (function
        | None -> (
            match f None with None -> None | Some w -> Some [ (k, w) ])
        | Some old -> _nonempty (Lmap.change (fun _ -> f) k old))
      h m

  let add_change (add : 'b -> 'a) (change : 'b -> 'a -> 'a) k x t =
    Intmap.change
      (function
        | None -> Some (Lmap.singleton k (add x))
        | Some l -> _nonempty @@ Lmap.add_change add change k x l)
      (K.hash k) t

  (* good sharing *)
  let add k v = add_change (fun x -> x) (fun x _ -> x) k v
  let add_new exn k v = add_change (fun x -> x) (fun _ _ -> raise exn) k v
  let find k m = Lmap.find k (Intmap.find (K.hash k) m)
  let find_opt k m = try Some (find k m) with Stdlib.Not_found -> None
  let find_def def k m = try find k m with Stdlib.Not_found -> def
  let find_exn exn k m = try find k m with Stdlib.Not_found -> raise exn

  let mem k m =
    try
      ignore (find k m);
      true
    with Stdlib.Not_found -> false

  let map f m = Intmap.map (Lmap.map f) m
  let mapi f m = Intmap.map (Lmap.mapi f) m

  let filter f m =
    Intmap.map_filter
      (fun l ->
        let l = Lmap.filter f l in
        if List.is_empty l then None else Some l)
      m

  (* good sharing *)
  let remove k m =
    let h = K.hash k in
    Intmap.change
      (function None -> None | Some old -> _nonempty (Lmap.remove k old))
      h m

  let find_remove k m =
    try
      let v = find k m in
      (remove k m, Some v)
    with Stdlib.Not_found -> (m, None)

  let iter f m = Intmap.iter (fun _ -> Lmap.iter f) m
  let fold f m a = Intmap.fold (fun _ -> Lmap.fold f) m a
  let fold_left f a m = Intmap.fold_left (fun a _ v -> Lmap.fold_left f a v) a m
  let size m = fold (fun _ _ w -> w + 1) m 0

  exception NotImplemented

  (* good sharing *)
  let partition _ = raise NotImplemented

  (* good sharing *)
  let union f = Intmap.union (fun _h m1 m2 -> _nonempty @@ Lmap.union f m1 m2)
  let inter f = Intmap.inter (fun _h m1 m2 -> _nonempty @@ Lmap.interf f m1 m2)
  let diff f = Intmap.diff (fun _ l1 l2 -> _nonempty @@ Lmap.diffq f l1 l2)

  (* good sharing *)
  let submap f = Intmap.submap (fun _h -> Lmap.subset f)

  (* good sharing *)
  let merge f =
    Intmap.merge (fun _h u1 u2 ->
        _nonempty (Lmap.merge f (_nonempty_inv u1) (_nonempty_inv u2)))

  let union_merge f m1 m2 =
    Intmap.union_merge
      (fun _ m1 m2 ->
        match m1 with
        | None -> _nonempty @@ Lmap.filter_mapi (fun k v2 -> f k None v2) m2
        | Some m1 -> _nonempty @@ Lmap.union_merge f m1 m2)
      m1 m2

  let of_list l =
    List.fold_left ~f:(fun acc (k, d) -> add k d acc) ~init:empty l

  let mapl f m = fold (fun k v a -> f k v :: a) m []
  let bindings m = mapl (fun k v -> (k, v)) m
  let values m = mapl (fun _ v -> v) m
  let keys m = mapl (fun k _ -> k) m
  let set_union m1 m2 = union (fun _ x _ -> Some x) m1 m2

  let pp data fmt m =
    Fmt.iter_bindings ~sep:Fmt.semi iter
      (Fmt.pair ~sep:Fmt.comma K.pp data)
      fmt m

  let for_all f m =
    Intmap.for_all (fun _ l -> List.for_all ~f:(fun (k, v) -> f k v) l) m

  let exists f m =
    Intmap.exists (fun _ l -> List.exists ~f:(fun (k, v) -> f k v) l) m

  let translate f m = fold_left (fun a k v -> add (f k) v a) empty m

  let disjoint f m1 m2 =
    Intmap.disjoint (fun _ l1 l2 -> Lmap.disjoint f l1 l2) m1 m2

  let choose m =
    match Intmap.choose m with
    | _, [] -> raise Stdlib.Not_found
    | _, (k, v) :: _ -> (k, v)

  exception IsNumElt

  let is_num_elt n m =
    try
      let n =
        fold_left (fun n _ _ -> if n = 0 then raise IsNumElt else n - 1) n m
      in
      n = 0
    with IsNumElt -> false

  let fold2_inter f m1 m2 acc =
    Intmap.fold2_inter
      (fun _ l1 l2 acc -> Lmap.fold2_inter f l1 l2 acc)
      m1 m2 acc

  let set_equal m1 m2 = equal (fun _ _ -> true) m1 m2
  let set_diff m1 m2 = diff (fun _ _ _ -> None) m1 m2
  let set_inter m1 m2 = inter (fun _ x _ -> Some x) m1 m2
  let set_submap m1 m2 = submap (fun _ _ _ -> true) m1 m2
  let set_disjoint m1 m2 = disjoint (fun _ _ _ -> false) m1 m2
  let set_compare m1 m2 = compare (fun _ _ -> 0) m1 m2
  let choose_rnd _ = raise NotImplemented
  let fold_decr _ = raise NotImplemented
  let fold2_union _ = raise NotImplemented
  let mapi_fold _ = raise NotImplemented
  let mapi_filter_fold _ = raise NotImplemented
  let add_opt _ = raise NotImplemented
  let map_filter _ = raise NotImplemented
  let find_smaller_opt _ = raise NotImplemented
  let mapi_filter _ = raise NotImplemented
  let split _ = raise NotImplemented
  let cardinal = size
  let min_binding _ = raise NotImplemented
  let max_binding _ = raise NotImplemented

  type 'a enumeration =
    | End
    | E of K.t * 'a * 'a Lmap.t * 'a Lmap.t Intmap.enumeration

  let next_ge_enum _ = raise NotImplemented
  let start_ge_enum _ = raise NotImplemented

  let rec get_next e =
    match Intmap.val_enum e with
    | None -> End
    | Some (_, []) -> get_next (Intmap.next_enum e)
    | Some (_, (k, v) :: l) -> E (k, v, l, e)

  let next_enum = function
    | End -> End
    | E (_, _, [], e) -> get_next (Intmap.next_enum e)
    | E (_, _, (k, v) :: l, e) -> E (k, v, l, e)

  let start_enum m = get_next (Intmap.start_enum m)
  let val_enum = function End -> None | E (k, v, _, _) -> Some (k, v)
end
