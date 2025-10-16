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
(** Merging List-Association Functor *)
(* -------------------------------------------------------------------------- *)

module Make (K : Map_intf.OrderedHashedType) : sig
  type key = K.t
  type 'a t = (key * 'a) list [@@deriving hash]

  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  val empty : 'a t
  val is_empty : 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val findk : key -> 'a t -> key * 'a
  val remove : key -> 'a t -> 'a t
  val change : (key -> 'a option -> 'a option) -> key -> 'a t -> 'a t

  val add_change :
    ('b -> 'a) ->
    ('b -> 'a -> 'a) ->
    key ->
    'b ->
    (key * 'a) list ->
    (key * 'a) list

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val filter_mapi : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val mapf : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val mapq : (key -> 'a -> 'a option) -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_left : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val inter : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val interf : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val interq : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diffq : (key -> 'a -> 'b -> 'a option) -> 'a t -> 'b t -> 'a t
  val subset : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val fold2_inter : (key -> 'a -> 'c -> 'b -> 'b) -> 'a t -> 'c t -> 'b -> 'b
  val iterk : (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val iter2 : (key -> 'a option -> 'b option -> unit) -> 'a t -> 'b t -> unit

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val disjoint : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool

  val union_merge :
    (key -> 'a option -> 'b -> 'a option) -> 'a t -> 'b t -> 'a t
end
