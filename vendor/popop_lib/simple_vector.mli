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

type 'a t
(** Plain and simple imperative and extensible Arrays *)

val create : int -> 'a t
val size : 'a t -> int
val get : 'a t -> int -> 'a
val get_def : 'a t -> int -> 'a -> 'a
val set : 'a t -> int -> 'a -> unit
val is_uninitialized : 'a t -> int -> bool
val uninitialize : 'a t -> int -> unit

val clear : 'a t -> unit
(** uninitialize all without allocating a new array *)

val init_inc_size : int -> (int -> 'a) -> 'a t -> unit
val inc_size : int -> 'a t -> unit
val iter_initialized : ('a -> unit) -> 'a t -> unit
val fold_initialized : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val apply_initialized : ('a -> 'a) -> 'a t -> unit
val iter_initializedi : (int -> 'a -> unit) -> 'a t -> unit
val fold_initializedi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
val copy : 'a t -> 'a t
(* shallow *)

val move : from:'a t -> to_:'a t -> unit
(* transfer the data *)

val push : 'a t -> 'a -> unit
(** used as a stack, put the element at the end of the array *)

val drop_last : 'a t -> unit
val decrease_size_to : 'a t -> int -> unit

val get_dumb : 'a t -> int -> 'a
(** If you know the implementation *)

val dumb : 'a
