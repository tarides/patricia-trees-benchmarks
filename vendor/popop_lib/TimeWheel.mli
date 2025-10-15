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

(** Time Wheel *)

(** Allows to add timestamp in the futur and get the next timestamp.

    It should be (but not proved) amortized O(log(offset)) for adding an event
    to an offset in the futur and O(1) amortized to get the next elements *)

module type S = sig
  type 'a t
  type context

  val create : context -> 'a t

  val add : 'a t -> 'a -> int -> unit
  (** [add t v offset] add the event v at the given offset in the futur *)

  val next : 'a t -> 'a option
  val next_at_same_time : 'a t -> 'a option
  val find_next : 'a t -> unit
  val current_time : 'a t -> int
  val size : 'a t -> int
  val size_at_current_time : 'a t -> int
end

include S with type context := unit

module Make (Context : sig
  type t
end) (_ : sig
  type 'a t

  val create : Context.t -> int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end) (_ : sig
  type 'a t

  val create : Context.t -> 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end) : S with type context := Context.t
