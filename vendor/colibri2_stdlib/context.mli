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

(** Context and backtrack point management *)

type context
(** A context, with an history of backtrack point *)

type creator
(** Same than context, but only used for creating datastructure *)

val creator : context -> creator

type bp
(** A backtrack point associated to a context *)

val create : unit -> context
(** Create a new context, with a base backtrack point. It is not possible to go
    below this backtrack point. *)

val bp : context -> bp
(** Get the current backtrack point *)

val push : context -> unit
(** Push a new backtrack point *)

exception AlreadyPoped

val pop : bp -> unit
(** Pop the context associated to this backtrack point to this backtrack point.
    All the backtrack point created since the given backtrack point are also
    poped.

    raise AlreadyPoped if it already has been poped. *)

val do_when_poping_current_bp : creator -> (unit -> unit) -> unit
(** Add a function called when the current context is popped *)

val always_do_when_pushing : creator -> (unit -> unit) -> unit
(** Add a function called aftera backtrack point is added *)

val always_do_before_pushing : creator -> (unit -> unit) -> unit
(** Add a function called before a backtrack point is added *)

module Ref : sig
  type 'a t
  (** A reference aware of a context *)

  val create : creator -> 'a -> 'a t
  (** Create a reference in this context with the given value *)

  val set : 'a t -> 'a -> unit
  (** Modify the reference *)

  val get : 'a t -> 'a
  (** Get the current value of the reference *)

  val incr : ?trace:string -> int t -> unit
  (** Increment the current value of the reference *)

  val creator : 'a t -> creator
  val pp : 'a Fmt.t -> 'a t Fmt.t
end

module RefOpt : sig
  type 'a t

  val create : creator -> 'a t
  val set : 'a t -> 'a -> unit
  val unset : 'a t -> unit
  val get : 'a t -> 'a option
end

type 'a history
(** history of the values *)

module Make (S : sig
  type t
  (** a type to make context aware *)

  type saved
  (** The data to save at backtrack point *)

  val save : t -> saved
  (** Get the data to save from the original type *)

  val restore : saved -> t -> unit
  (** Restore the saved data after a pop (delayed at the next {!refresh}) *)

  val get_history : t -> saved history
end) : sig
  val create : creator -> S.saved history
  (** Create an history *)

  val refresh : S.t -> unit
  (** Function to call before accessing the value when a pop could have occured
  *)

  val save : S.t -> unit
  (** Function to call before modifying the value, it does also refresh *)

  type hidden
  (** To be used for enforcing the use of the previous function *)

  val ro : hidden -> S.t
  val rw : hidden -> S.t
  val hide : S.t -> hidden
  val creator : 'a history -> creator
end

module Basic (S : sig
  type t
  (** a type to make context aware *)

  type saved
  (** The data to save at backtrack point *)

  val save : t -> saved
  (** Get the data to save from the original type *)

  val restore : saved -> t -> unit
  (** Restore the saved data after a pop (delayed at the next {!refresh}) *)
end) : sig
  type t

  val create : creator -> S.t -> t
  (** Create an history *)

  val get : t -> S.t
end

module Expert : sig
  type 'a history

  val refresh : restore:('b -> 'a -> unit) -> 'b -> 'a history -> unit

  val save :
    restore:('b -> 'a -> unit) -> save:('b -> 'a) -> 'b -> 'a history -> unit

  val create : creator -> 'a history
end

module Push : sig
  type 'a t

  val create : creator -> 'a t
  val push : 'a t -> 'a -> unit
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val exists : ('a -> bool) -> 'a t -> bool
  val length : 'a t -> int
  val get : 'a t -> int -> 'a

  val to_seq : 'a t -> 'a Base.Sequence.t
  (** From last current element to first *)

  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end

module Queue : sig
  (** The saving is equal to the current size of the datastructure. Should be
      used when usually it is empty at backtrack point *)

  type 'a t

  val create : creator -> 'a t
  val enqueue : 'a t -> 'a -> unit
  val dequeue : 'a t -> 'a option
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val is_empty : 'a t -> bool

  val of_seq : 'a t -> 'a Base.Sequence.t
  (** From last current element to first *)

  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end

module Array : sig
  type 'a t

  val create : creator -> int -> 'a -> 'a t
  val set : 'a t -> int -> 'a -> unit
  val get : 'a t -> int -> 'a
end

module TimeWheel : Colibri2_popop_lib.TimeWheel.S with type context := creator

module type Hashtbl = sig
  type 'a t
  type key

  val pp : 'a Fmt.t -> 'a t Fmt.t
  val create : creator -> 'a t
  val remove : 'a t -> key -> unit
  val set : 'a t -> key -> 'a -> unit
  val set_opt : 'a t -> key -> 'a option -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_def : 'a t -> def:'a -> key -> 'a
  val find_exn : 'a t -> exn -> key -> 'a
  val find_remove : 'a t -> key -> 'a option
  val mem : 'a t -> key -> bool
  val change : ('a option -> 'a option) -> 'a t -> key -> unit
  val add_change : ('b -> 'a) -> ('b -> 'a -> 'a) -> 'a t -> key -> 'b -> unit
  val choose : 'a t -> (key * 'a) option
  val iter : f:(key -> 'a -> unit) -> 'a t -> unit
  val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val clear : 'a t -> unit
  val length : 'a t -> int
end

module Hashtbl (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  Hashtbl with type key := S.t

module type HashtblWithoutRemove = sig
  type 'a t
  type key

  val pp : 'a Fmt.t -> 'a t Fmt.t
  val create : creator -> 'a t
  val set : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_def : 'a t -> def:'a -> key -> 'a
  val find_exn : 'a t -> exn -> key -> 'a
  val mem : 'a t -> key -> bool
  val change : ('a option -> 'a) -> 'a t -> key -> unit
  val add_change : ('b -> 'a) -> ('b -> 'a -> 'a) -> 'a t -> key -> 'b -> unit
  val choose : 'a t -> (key * 'a) option
  val iter : f:(key -> 'a -> unit) -> 'a t -> unit
  val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val filter_map_inplace : (key -> 'a -> 'a) -> 'a t -> unit
  val length : 'a t -> int
end

module HashtblWithoutRemove (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  HashtblWithoutRemove with type key := S.t

module type HashtblWithDefault = sig
  type 'a t
  type key

  val create : creator -> (creator -> 'a) -> 'a t
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val set : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val change : ('a -> 'a) -> 'a t -> key -> unit
end

module HashtblWithDefault (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  HashtblWithDefault with type key := S.t

module type Memo = sig
  type 'a t
  type key

  val length : 'a t -> int
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val create : creator -> (creator -> key -> 'a) -> 'a t
  val find : 'a t -> key -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
end

module Memo (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  Memo with type key := S.t

(** Clicket *)

(** This datastructure as a behavior very different from the others. Instead of
    resetting to its previous state after a pop, new elements have been added.
    The element added after a backtrack point are given back after each pop. *)

module type Clicket = sig
  type 'a t

  val create : creator -> 'a t
  val push : 'a t -> 'a -> unit

  val iter : f:('a -> unit) -> 'a t -> unit
  (** iter on the one that have been added inside sub-branch *)

  val todo : 'a t -> bool
  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end

module Clicket : Clicket

type 'k fold = { fold : 'a. ('a -> 'k -> 'a) -> 'a -> 'a }

module type Trie = sig
  type 'a t
  type key

  val create : creator -> 'a t
  val pp : 'a Fmt.t -> 'a t Fmt.t

  module List : sig
    val set : 'a t -> key list -> 'a -> unit
    val find_def : default:(creator -> 'a) -> 'a t -> key list -> 'a
  end

  module Set : sig
    type set

    val set : 'a t -> set -> 'a -> unit
    val find_def : default:(creator -> 'a) -> 'a t -> set -> 'a
  end

  module Fold : sig
    val set : 'a t -> key fold -> 'a -> unit
    val find_def : default:(creator -> 'a) -> 'a t -> key fold -> 'a

    val memo : default:(creator -> 'a) -> 'a t -> key fold -> 'a
    (** find and add default if not present *)
  end
end

module Trie (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  Trie with type key := S.t and type Set.set := S.S.t
