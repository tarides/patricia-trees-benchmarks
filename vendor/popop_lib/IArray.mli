(*************************************************************************)
(*  This file is part of Colibri2.                                       *)
(*                                                                       *)
(*  Copyright (C) 2014-2021                                              *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*                                                                       *)
(*  you can redistribute it and/or modify it under the terms of the GNU  *)
(*  Lesser General Public License as published by the Free Software      *)
(*  Foundation, version 2.1.                                             *)
(*                                                                       *)
(*  It is distributed in the hope that it will be useful,                *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*  GNU Lesser General Public License for more details.                  *)
(*                                                                       *)
(*  See the GNU Lesser General Public License version 2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).           *)
(*************************************************************************)

(** immutable arrays, like arrays but you can't modify them after creation *)

type 'a t

val of_list : 'a list -> 'a t
val of_list_map : f:('a -> 'b) -> 'a list -> 'b t
val of_array : 'a array -> 'a t
val of_array_map : f:('a -> 'b) -> 'a array -> 'b t
val empty : 'a t
val is_empty : 'a t -> bool
val not_empty : 'a t -> bool
val length : 'a t -> int
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val get : 'a t -> int -> 'a
val hash : ('a -> int) -> 'a t -> int
val hash_fold_t : 'a Base.Hash.folder -> 'a t Base.Hash.folder
val iter : f:('a -> unit) -> 'a t -> unit
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

val foldi_non_empty_exn :
  init:('a -> 'b) -> f:(int -> 'b -> 'a -> 'b) -> 'a t -> 'b
(** The accumulator is obtained with the first elements *)

val fold2_exn :
  init:'acc -> f:('acc -> 'a -> 'b -> 'acc) -> 'a t -> 'b t -> 'acc

val for_alli : f:(int -> 'a -> bool) -> 'a t -> bool
val for_all2_exn : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val for_alli_non_empty_exn :
  init:('a -> 'acc) -> f:(int -> 'acc -> 'a -> bool) -> 'a t -> bool

val map : f:('a -> 'b) -> 'a t -> 'b t
val map2_exn : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val pp : ?sep:unit Pp.pp -> 'a Pp.pp -> 'a t Pp.pp
val extract1_exn : 'a t -> 'a
val extract2_exn : 'a t -> 'a * 'a
val extract3_exn : 'a t -> 'a * 'a * 'a
val extract4_exn : 'a t -> 'a * 'a * 'a * 'a
val to_list : 'a t -> 'a list
val to_seq : 'a t -> 'a Base.Sequence.t
val mk1 : 'a -> 'a t
val mk2 : 'a -> 'a -> 'a t
