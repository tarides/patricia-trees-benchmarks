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
(* --- Merging List-map Functor                                           --- *)
(* -------------------------------------------------------------------------- *)

open Base

module Make (K : Map_intf.OrderedHashedType) = struct
  (* Should be implemented using listset *)

  type key = K.t
  type 'a t = (K.t * 'a) list [@@deriving hash]

  let rec equal eq t1 t2 =
    match (t1, t2) with
    | [], [] -> true
    | (k1, x1) :: t1, (k2, x2) :: t2 ->
        K.equal k1 k2 && eq x1 x2 && equal eq t1 t2
    | _ -> false

  let rec compare cmp a b =
    match (a, b) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | (k1, v1) :: xs, (k2, v2) :: ys ->
        let n = K.compare k1 k2 in
        if n = 0 then
          let n = cmp v1 v2 in
          if n = 0 then compare cmp xs ys else n
        else n

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let singleton k v = [ (k, v) ]

  (* used for better sharing between a list and a modified list *)
  let rev_append_until i l1 l2 =
    let rec aux acc = function
      | [] -> acc
      | i' :: _ when phys_equal i' i -> acc
      | i' :: l -> aux (i' :: acc) l
    in
    aux l2 l1

  (* used for better sharing between a list and a modified list *)
  let append_until i l1 l2 = List.rev_append (rev_append_until i l1 []) l2

  (** good sharing, non tail reccursive *)
  let rec mapq f l =
    match l with
    | [] -> l
    | ((k, v) as i) :: resti -> (
        match f k v with
        | None -> mapq f resti
        | Some v' ->
            if phys_equal v' v then
              let resti' = mapq f resti in
              if phys_equal resti' resti then l else i :: resti
            else (* add new *)
              (k, v') :: mapq f resti)

  (* good sharing *)
  (* idem List.filter, but returns l if no element is removed. *)
  let filter f l =
    let rec aux ((res, rest) as acc) = function
      | [] -> List.rev_append res rest
      | i :: resti ->
          if f i then (* add idem *) aux acc resti
          else (* remove *) aux (rev_append_until i rest res, resti) resti
    in
    aux ([], l) l

  (* good sharing *)
  (* idem List.partition, better sharing. *)
  let partition f l =
    let rec aux ((res, rest) as acc) ((res', rest') as acc') = function
      | [] -> (List.rev_append res rest, List.rev_append res' rest')
      | ((k, v) as i) :: resti ->
          if f k v then aux acc (rev_append_until i rest' res', resti) resti
          else aux (rev_append_until i rest res, resti) acc' resti
    in
    aux ([], l) ([], l) l

  (* good sharing *)
  let change f k l =
    let rec aux = function
      | [] -> ( match f k None with None -> l | Some w -> l @ [ (k, w) ])
      | ((k', v') as a) :: next ->
          let c = K.compare k k' in
          if c < 0 then l
          else if c = 0 then
            match f k (Some v') with
            | None -> append_until a l next
            | Some w ->
                if phys_equal w v' then l else append_until a l ((k, w) :: next)
          else (* c > 0 *) aux next
    in
    aux l

  let rec add_change (add : 'b -> 'a) (change : 'b -> 'a -> 'a) k x t =
    match t with
    | [] -> [ (k, add x) ]
    | ((k', v') as kv') :: t' ->
        let c = K.compare k k' in
        if c < 0 then (k, add x) :: t
        else if c = 0 then
          let v = change x v' in
          if phys_equal v v' then t else (k, v) :: t'
        else
          let r' = add_change add change k x t' in
          if phys_equal r' t' then t else kv' :: r'

  (* good sharing *)
  let insert f k v l =
    let rec aux = function
      | [] -> l @ [ (k, v) ]
      | ((k', v') as a) :: next as w ->
          let c = K.compare k k' in
          if c < 0 then append_until a l ((k, v) :: w)
          else if c = 0 then
            let w = f k v v' in
            if phys_equal w v' then l else append_until a l ((k, w) :: next)
          else (* c > 0 *) aux next
    in
    aux l

  (* good sharing *)
  let add k x = insert (fun _k x _old -> x) k x

  let rec findk k = function
    | [] -> raise Stdlib.Not_found
    | ((k0, _) as e) :: next ->
        let c = K.compare k k0 in
        if c < 0 then raise Stdlib.Not_found
        else if c > 0 then findk k next
        else e

  let find k m = snd (findk k m)

  let mem k m =
    try
      ignore (find k m);
      true
    with Stdlib.Not_found -> false

  let mapi f = List.map ~f:(fun (k, v) -> (k, f k v))
  let map f = mapi (fun _k v -> f v)
  let iter f = List.iter ~f:(fun (k, v) -> f k v)

  let filter_map f =
    List.filter_map ~f:(fun (k, v) ->
        match f v with Some v -> Some (k, v) | None -> None)

  let filter_mapi f =
    List.filter_map ~f:(fun (k, v) ->
        match f k v with Some v -> Some (k, v) | None -> None)

  (* good sharing *)
  let remove k m = change (fun _ _ -> None) k m

  (* good sharing *)
  let filter f m = filter (fun (k, x) -> f k x) m

  let rec mapf f = function
    | [] -> []
    | (k, x) :: m -> (
        match f k x with Some y -> (k, y) :: mapf f m | None -> mapf f m)

  let fold f m a = List.fold_left ~f:(fun a (k, v) -> f k v a) ~init:a m
  let fold_left f a m = List.fold_left ~f:(fun a (k, v) -> f a k v) ~init:a m

  let rec inter f w1 w2 =
    match (w1, w2) with
    | [], _ | _, [] -> []
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then inter f r1 w2
        else if c > 0 then inter f w1 r2
        else (k1, f k1 v1 v2) :: inter f r1 r2

  let rec interf f w1 w2 =
    match (w1, w2) with
    | [], _ | _, [] -> []
    | (k1, v1) :: r1, (k2, v2) :: r2 -> (
        let c = K.compare k1 k2 in
        if c < 0 then interf f r1 w2
        else if c > 0 then interf f w1 r2
        else
          match f k1 v1 v2 with
          | None -> interf f r1 r2
          | Some v12 -> (k1, v12) :: interf f r1 r2)

  (* good sharing with w1 *)
  let interq f w1 w2 =
    let rec aux ((res, o1) as acc) w1 w2 =
      match (w1, w2) with
      | [], _ -> (* no addition *) List.rev_append res o1
      | a1 :: _, [] ->
          (* no addition *)
          List.rev_append res (List.rev (rev_append_until a1 o1 []))
      | ((k1, v1) as a1) :: r1, (k2, v2) :: r2 -> (
          let c = K.compare k1 k2 in
          if c < 0 then
            (* remove a1 *)
            aux (rev_append_until a1 o1 res, r1) r1 w2
          else if c > 0 then (* remove a2 *) aux acc w1 r2
          else
            match f k1 v1 v2 with
            | None -> (* remove a1 *) aux (rev_append_until a1 o1 res, r1) r1 r2
            | Some w ->
                if phys_equal w v1 then (* adding a1 *) aux acc r1 r2
                else
                  (* adding w *)
                  aux ((k1, w) :: rev_append_until a1 o1 res, r1) r1 r2)
    in
    aux ([], w1) w1 w2

  (* good sharing with w1 *)
  let diffq f w1 w2 =
    let rec aux ((res, o1) as acc) w1 w2 =
      match (w1, w2) with
      | [], _ -> (* no addition *) List.rev_append res o1
      | _, [] -> (* adding w1 *) List.rev_append res o1
      | ((k1, v1) as a1) :: r1, (k2, v2) :: r2 -> (
          let c = K.compare k1 k2 in
          if c < 0 then (* adding a1 *) aux acc r1 w2
          else if c > 0 then (* skip *) aux acc w1 r2
          else
            match f k1 v1 v2 with
            | None -> (* remove a1 *) aux (rev_append_until a1 o1 res, r1) r1 r2
            | Some w ->
                if phys_equal w v1 then (* adding a1 *) aux acc r1 r2
                else
                  (* adding w *)
                  aux ((k1, w) :: rev_append_until a1 o1 res, r1) r1 r2)
    in
    aux ([], w1) w1 w2

  (* good sharing with w1 *)
  let union f w1 w2 =
    let rec aux ((res, o1) as acc) w1 w2 =
      match (w1, w2) with
      | [], _ -> (* adding w2 *) List.rev_append res (List.append o1 w2)
      | _, [] -> (* adding w1 *) List.rev_append res o1
      | ((k1, v1) as a1) :: r1, ((k2, v2) as a2) :: r2 ->
          let c = K.compare k1 k2 in
          if c < 0 then (* adding a1 *) aux acc r1 w2
          else if c = 0 then
            match f k1 v1 v2 with
            | None -> aux (rev_append_until a1 o1 res, r1) r1 r2
            | Some w ->
                if phys_equal w v1 then (* adding a1 *) aux acc r1 r2
                else
                  (* adding w *)
                  aux ((k1, w) :: rev_append_until a1 o1 res, r1) r1 r2
          else
            (* c > 0 *)
            (* adding a2 *) aux (a2 :: rev_append_until a1 o1 res, w1) w1 r2
    in
    aux ([], w1) w1 w2

  let rec subset f w1 w2 =
    match (w1, w2) with
    | [], _ -> true
    | _ :: _, [] -> false
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then false
        else if c > 0 then subset f w1 r2
        else f k1 v1 v2 && subset f r1 r2

  let rec iterk (f : K.t -> 'a -> 'b -> unit) (w1 : (K.t * 'a) list)
      (w2 : (K.t * 'b) list) =
    match (w1, w2) with
    | [], _ | _, [] -> ()
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then iterk f r1 w2
        else if c > 0 then iterk f w1 r2
        else (
          f k1 v1 v2;
          iterk f r1 r2)

  let rec iter2 (f : K.t -> 'a option -> 'b option -> unit)
      (w1 : (K.t * 'a) list) (w2 : (K.t * 'b) list) =
    match (w1, w2) with
    | [], [] -> ()
    | _, [] -> List.iter ~f:(fun (k1, v1) -> f k1 (Some v1) None) w1
    | [], _ -> List.iter ~f:(fun (k2, v2) -> f k2 None (Some v2)) w2
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then (
          f k1 (Some v1) None;
          iter2 f r1 w2)
        else if c > 0 then (
          f k2 None (Some v2);
          iter2 f w1 r2)
        else (
          f k1 (Some v1) (Some v2);
          iter2 f r1 r2)

  let rec disjoint (f : K.t -> 'a -> 'b -> bool) (w1 : (K.t * 'a) list)
      (w2 : (K.t * 'b) list) =
    match (w1, w2) with
    | [], [] -> true
    | _, [] -> true
    | [], _ -> true
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then disjoint f r1 w2
        else if c > 0 then disjoint f w1 r2
        else f k1 v1 v2 && disjoint f r1 r2

  let cons k v w = match v with None -> w | Some x -> (k, x) :: w

  let rec merge (f : K.t -> 'a option -> 'b option -> 'c option) w1 w2 =
    match (w1, w2) with
    | [], [] -> []
    | _, [] -> mapf (fun k1 v1 -> f k1 (Some v1) None) w1
    | [], _ -> mapf (fun k2 v2 -> f k2 None (Some v2)) w2
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then cons k1 (f k1 (Some v1) None) (merge f r1 w2)
        else if c > 0 then cons k2 (f k2 None (Some v2)) (merge f w1 r2)
        else cons k1 (f k1 (Some v1) (Some v2)) (merge f r1 r2)

  let rec fold2_inter f w1 w2 acc =
    match (w1, w2) with
    | [], _ | _, [] -> acc
    | (k1, v1) :: r1, (k2, v2) :: r2 ->
        let c = K.compare k1 k2 in
        if c < 0 then fold2_inter f r1 w2 acc
        else if c > 0 then fold2_inter f w1 r2 acc
        else fold2_inter f r1 r2 (f k1 v1 v2 acc)

  let union_merge f l1 l2 =
    merge
      (fun k v1 v2 -> match v2 with None -> v1 | Some v2 -> f k v1 v2)
      l1 l2
end
