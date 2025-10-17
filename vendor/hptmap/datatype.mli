(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A datatype provides useful values for types. It is a high-level API on top
    of module {!Type}.
    @since Carbon-20101201
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ********************************************************************** *)
(** {2 Type declarations} *)
(* ********************************************************************** *)

(** Values associated to each datatype.
    Some others are provided directly in module {!Type}.
    @before 26.0-Iron there was additional fields only used for Journalization
            that has been removed.
*)
type 'a t = private
  { equal: 'a -> 'a -> bool;
    compare: 'a -> 'a -> int;
    hash: 'a -> int;
    copy: 'a -> 'a;
    pretty: Format.formatter -> 'a -> unit;
    (* mem_project: (Project_skeleton.t -> bool) -> 'a -> bool *) 
    }

(** A type with its type value. *)
module type Ty = sig
  type t
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  (* val ty: t Type.t *)
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
end

(** All values associated to a datatype, excepted [copy].
    @before 26.0-Iron there was several additional values only used for
            Journalization that has been removed.
*)
module type S_no_copy = sig

  include Ty

  val datatype_name: string
  (** Unique name of the datatype. *)

  (* val datatype_descr: t Descr.t *)
  (** Datatype descriptor. *)

  (* val packed_descr: Structural_descr.pack *)
  (** Packed version of the descriptor. *)

  (* val reprs: t list *)
  (** List of representants of the descriptor. *)

  val equal: t -> t -> bool
  (** Equality: same spec than [Stdlib.(=)].
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
  *)

  val compare: t -> t -> int
  (** Comparison: same spec than [Stdlib.compare]. *)

  val hash: t -> int
  (** Hash function: same spec than [Hashtbl.hash]. *)

  (* val pretty: Format.formatter -> t -> unit *)
  (** Pretty print each value in an user-friendly way.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
  *)

  (* val mem_project: (Project_skeleton.t -> bool) -> t -> bool *)
  (** [mem_project f x] must return [true] iff there is a value [p] of type
      [Project.t] in [x] such that [f p] returns [true]. *)

end

(** All values associated to a datatype. *)
module type S = sig
  include S_no_copy
  val copy: t -> t
  (** Deep copy: no possible sharing between [x] and [copy x]. *)
end

(* ********************************************************************** *)
(** {2 Getters from a type value} *)
(* ********************************************************************** *)

(* val info: 'a Type.t -> 'a t *)
(* val equal: 'a Type.t -> 'a -> 'a -> bool *)
(* val compare: 'a Type.t -> 'a -> 'a -> int *)
(* val hash: 'a Type.t -> 'a -> int *)
(* val copy: 'a Type.t -> 'a -> 'a *)
(* val pretty: 'a Type.t -> Format.formatter -> 'a -> unit *)
(* val mem_project: 'a Type.t -> (Project_skeleton.t -> bool) -> 'a -> bool *)

(* ********************************************************************** *)
(** {2 Easy builders} *)
(* ********************************************************************** *)

val undefined: 'a -> 'b
(** Must be used if you don't want to implement a required function.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val identity: 'a -> 'a
(** Must be used if you want to implement a required function by [fun x ->
    x]. Only useful for implementing [rehash] and [copy].
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val from_compare: 'a -> 'a -> bool
(** Must be used for [equal] in order to implement it by [compare x y = 0]
    (with your own [compare] function). *)

(* val never_any_project: (Project_skeleton.t -> bool) -> 'a -> bool *)
(** Must be used for [mem_project] if values of your type does never contain
    any project.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(** Sub-signature of {!S}.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>

    @before 26.0-Iron there was several additional values only used for
            Journalization that has been removed.
*)
module type Undefined = sig
  (* val structural_descr: Structural_descr.t *)
  val equal: 'a -> 'a -> bool
  val compare: 'a -> 'a -> int
  val hash: 'a -> int
  val rehash: 'a -> 'a
  val copy: 'a -> 'a
  (* val pretty: Format.formatter -> 'a -> unit *)
  (* val mem_project: (Project_skeleton.t -> bool) -> 'a -> bool *)
end

(** Each values in these modules are undefined. The usual way to use it is:
    [module X: Datatype.S = struct
    include Undefined
    type t = ...
    let reprs = ...
    let name = ...
    let mem_project = ... (* Usually, Datatype.never_any_project *)
    (* define only useful functions for this datatype *)
    end] *)
module Undefined: Undefined

(** Same as {!module:Undefined}, but the type is supposed to be marshallable by the
    standard OCaml way (in particular, no hash-consing or projects inside
    the type).
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Serializable_undefined: Undefined

(* ********************************************************************** *)
(** {2 Generic builders} *)
(* ********************************************************************** *)

(** Input signature of {!Make} and {!Make_with_collections}.
    Values to implement in order to get a datatype.
    Feel free to use easy builders (see above) for easy implementation.

    @before 26.0-Iron there was several additional values only used for
            Journalization that has been removed.
*)
module type Make_input = sig

  type t (** Type for this datatype *)

  val name: string
  (** Unique name for this datatype.
      If the name is a valid ocaml module name, then it must really corresponds
      to the module name you are defining by applying the functor.
      Otherwise, put the name you want as long as it does not clash with any
      other datatype name. *)

  val rehash: t -> t
  (** How to rehashconsed values. Must be {!identity} if you do not use
      hashconsing. Only useful for unmarshaling (use {!undefined} for
      unmarshable type). *)

  (** All the above operations have the same semantics than the corresponding
      value specified in module type {!S}. *)

  (* val structural_descr: Structural_descr.t *)
  (* val reprs: t list *)
  (** Must be non-empty.*)

  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val copy: t -> t
  (* val pretty: Format.formatter -> t -> unit *)
  (* val mem_project: (Project_skeleton.t -> bool) -> t -> bool *)

end

(** Generic datatype builder.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Make(X: Make_input): S with type t = X.t

(** A standard OCaml set signature extended with datatype operations. *)
module type Set = sig
  include Set.S
  val nearest_elt_le: elt -> t -> elt
  val nearest_elt_ge: elt -> t -> elt
  include S with type t := t
end

(** A standard OCaml map signature extended with datatype operations. *)
module type Map = sig

  include Map.S

  module Key: S with type t = key
  (** Datatype for the keys of the map. *)

  module Make(Data: S) : S with type t = Data.t t
  (** Build a datatype of the map according to the datatype of values in the
      map. *)
end

(** Marshallable collectors with hashtbl-like interface. *)
module type Hashtbl_with_descr = sig
  include Hashtbl.S
  (* val structural_descr: Structural_descr.t -> Structural_descr.t *)
end

(** A standard OCaml hashtbl signature extended with datatype operations. *)
module type Hashtbl = sig

  include Hashtbl_with_descr

  (* val make_type: 'a Type.t -> 'a t Type.t *)
  (** @since Fluorine-20130401 *)

  module Key: S with type t = key
  (** Datatype for the keys of the hashtbl. *)

  module Make(Data: S) : S with type t = Data.t t
  (** Build a datatype of the hashtbl according to the datatype of values in the
      hashtbl. *)

end

(** A datatype for a type [t] extended with predefined hashtbl over [t].
    @since 31.0-Gallium
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)
module type S_with_hashtbl = sig
  include S
  module Hashtbl: Hashtbl with type key = t
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
end

(** A datatype for a type [t] extended with predefined set and map over [t].
    @since 31.0-Gallium
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)
module type S_with_set_and_map = sig
  include S
  module Set: Set with type elt = t
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  module Map: Map with type key = t
end

(** A datatype for a type [t] extended with predefined set, map and hashtbl
    over [t].

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)
module type S_with_collections = sig
  include S
  module Set: Set with type elt = t
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

  module Map: Map with type key = t
  module Hashtbl: Hashtbl with type key = t
  (** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
end

(** Generic comparable datatype builder: functions [equal] and [compare] must
    not be {!undefined}.
    @since 31.0-Gallium *)
module Make_with_set_and_map(X: Make_input):
  S_with_set_and_map with type t = X.t

(** Add sets and maps to an existing datatype, provided the [equal] and
    [compare] are not {!undefined}.
    @since 31.0-Gallium *)
module With_set_and_map(X: S):
  S_with_set_and_map with type t = X.t

(** Generic comparable datatype builder: functions [equal] and [hash] must not
    be {!undefined}.
    @since 31.0-Gallium *)
module Make_with_hashtbl(X: Make_input):
  S_with_hashtbl with type t = X.t

(** Add hashtables modules to an existing datatype, provided the [equal] and
    [hash] functions are not {!undefined}.
    @since 31.0-Gallium *)
module With_hashtbl(X: S):
  S_with_hashtbl with type t = X.t

(** Generic comparable datatype builder: functions [equal], [compare] and
    [hash] must not be {!undefined}. *)
module Make_with_collections(X: Make_input):
  S_with_collections with type t = X.t

(** Add sets, maps and hashtables modules to an existing datatype, provided the
    [equal], [compare] and [hash] functions are not {!undefined}.
    @since Oxygen-20120901 *)
module With_collections(X: S):
  S_with_collections with type t = X.t

(* ****************************************************************************)
(** {2 Predefined datatype} *)
(* ****************************************************************************)

module Unit: S_with_collections with type t = unit
(* val unit: unit Type.t *)
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Bool: S_with_collections with type t = bool
(* val bool: bool Type.t *)
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Int: S_with_collections with type t = int
(* val int: int Type.t *)
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

module Int32: S_with_collections with type t = int32
(* val int32: int32 Type.t *)

module Int64: S_with_collections with type t = int64
(* val int64: int64 Type.t *)

module Nativeint: S_with_collections with type t = nativeint
(* val nativeint: nativeint Type.t *)

module Float: S_with_collections with type t = float
(* val float: float Type.t *)

module Char: S_with_collections with type t = char
(* val char: char Type.t *)
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module String: S_with_collections with type t = string
(* val string: string Type.t *)
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

module Formatter: S with type t = Format.formatter
(* val formatter: Format.formatter Type.t *)

module Integer: S_with_collections with type t = Integer.t
(* val integer: Integer.t Type.t *)

module Rational: S_with_collections with type t = Q.t
(* val rational : Rational.t Type.t *)

(* ****************************************************************************)
(** {2 Generic functors for polymorphic types} *)
(* ****************************************************************************)

(** Output signature of {!module:Polymorphic}. *)
(*module type Polymorphic = sig *)
(*  include Type.Polymorphic *)
(*  module Make(T: S) : S with type t = T.t poly *)
(*  (1** Create a datatype for a monomorphic instance of the polymorphic type. *1) *)
(*end *)

(** Functor for polymorphic types with only 1 type variable.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>

    @before 26.0-Iron the functor had several additional values only used for
            Journalization that has been removed.
*)
(* module Polymorphic *)
(*     (P: sig *)
(*        include Type.Polymorphic_input *)
(*        val mk_equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool *)
(*        val mk_compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int *)
(*        val mk_hash: ('a -> int) -> 'a t -> int *)
(*        val map: ('a -> 'b) -> 'a t -> 'b t *)
(*        val mk_pretty: *)
(*          (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit *)
(*        val mk_mem_project: *)
(*          ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*          (Project_skeleton.t -> bool) -> 'a t -> bool *)
(*      end) : *)
(*   Polymorphic with type 'a poly = 'a P.t *)

(** Output signature of {!module:Polymorphic2}. *)
(* module type Polymorphic2 = sig *)
(*   include Type.Polymorphic2 *)
(*   module Make(T1: S)(T2: S) : S with type t = (T1.t, T2.t) poly *)
(* end *)

(** Functor for polymorphic types with 2 type variables.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>

    @before 26.0-Iron the functor had several additional values only used for
            Journalization that has been removed.
*)
(* module Polymorphic2 *)
(*     (P: sig *)
(*        include Type.Polymorphic2_input *)
(*        val mk_equal: *)
(*          ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> *)
(*          bool *)
(*        val mk_compare: *)
(*          ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int *)
(*        val mk_hash: ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int *)
(*        val map: ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t *)
(*        val mk_pretty: *)
(*          (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> *)
(*          Format.formatter -> ('a, 'b) t -> unit *)
(*        val mk_mem_project: *)
(*          ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*          ((Project_skeleton.t -> bool) -> 'b -> bool) -> *)
(*          (Project_skeleton.t -> bool) -> ('a, 'b) t -> bool *)
(*      end) : *)
(*   Polymorphic2 with type ('a, 'b) poly = ('a, 'b) P.t *)

(** Output signature of {!module:Polymorphic3}.
    @since Oxygen-20120901 *)
(* module type Polymorphic3 = sig *)
(*   include Type.Polymorphic3 *)
(*   module Make(T1:S)(T2:S)(T3:S) : S with type t = (T1.t, T2.t, T3.t) poly *)
(* end *)

(** Functor for polymorphic types with 3 type variables.
    @since Oxygen-20120901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @before 26.0-Iron the functor had several additional values only used for
            Journalization that has been removed.
*)
(* module Polymorphic3 *)
(*     (P: sig *)
(*        include Type.Polymorphic3_input *)
(*        val mk_equal: *)
(*          ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('c -> 'c -> bool) -> *)
(*          ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> *)
(*          bool *)
(*        val mk_compare: *)
(*          ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('c -> 'c -> int) -> *)
(*          ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int *)
(*        val mk_hash: *)
(*          ('a -> int) -> ('b -> int) -> ('c -> int) -> ('a, 'b, 'c) t -> int *)
(*        val map: *)
(*          ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('a, 'c, 'e) t -> ('b, 'd, 'f) t *)
(*        val mk_pretty: *)
(*          (Format.formatter -> 'a -> unit) -> *)
(*          (Format.formatter -> 'b -> unit) -> *)
(*          (Format.formatter -> 'c -> unit) -> *)
(*          Format.formatter -> ('a, 'b, 'c) t -> unit *)
(*        val mk_mem_project: *)
(*          ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*          ((Project_skeleton.t -> bool) -> 'b -> bool) -> *)
(*          ((Project_skeleton.t -> bool) -> 'c -> bool) -> *)
(*          (Project_skeleton.t -> bool) -> ('a, 'b, 'c) t -> bool *)
(*      end) : *)
(*   Polymorphic3 with type ('a, 'b, 'c) poly = ('a, 'b, 'c) P.t *)

(** Output signature of {!module:Polymorphic4}.
    @since Oxygen-20120901 *)
(* module type Polymorphic4 = sig *)
(*   include Type.Polymorphic4 *)
(*   module Make(T1:S)(T2:S)(T3:S)(T4:S) *)
(*     : S with type t = (T1.t, T2.t, T3.t, T4.t) poly *)
(* end *)

(** Functor for polymorphic types with 4 type variables.
    @since Oxygen-20120901
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @before 26.0-Iron the functor had several additional values only used for
            Journalization that has been removed.
*)
(* module Polymorphic4 *)
(*     (P: sig *)
(*        include Type.Polymorphic4_input *)
(*        val mk_equal: *)
(*          ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> *)
(*          ('c -> 'c -> bool) -> ('d -> 'd -> bool) -> *)
(*          ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> *)
(*          bool *)
(*        val mk_compare: *)
(*          ('a -> 'a -> int) -> ('b -> 'b -> int) -> *)
(*          ('c -> 'c -> int) -> ('d -> 'd -> int) -> *)
(*          ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> int *)
(*        val mk_hash: *)
(*          ('a -> int) -> ('b -> int) -> ('c -> int) -> ('d -> int) -> *)
(*          ('a, 'b, 'c, 'd) t -> int *)
(*        val map: *)
(*          ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> *)
(*          ('a, 'c, 'e, 'g) t -> ('b, 'd, 'f, 'h) t *)
(*        val mk_pretty: *)
(*          (Format.formatter -> 'a -> unit) -> *)
(*          (Format.formatter -> 'b -> unit) -> *)
(*          (Format.formatter -> 'c -> unit) -> *)
(*          (Format.formatter -> 'd -> unit) -> *)
(*          Format.formatter -> ('a, 'b, 'c, 'd) t -> unit *)
(*        val mk_mem_project: *)
(*          ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*          ((Project_skeleton.t -> bool) -> 'b -> bool) -> *)
(*          ((Project_skeleton.t -> bool) -> 'c -> bool) -> *)
(*          ((Project_skeleton.t -> bool) -> 'd -> bool) -> *)
(*          (Project_skeleton.t -> bool) -> ('a, 'b, 'c, 'd) t -> bool *)
(*      end) : *)
(*   Polymorphic4 with type ('a, 'b, 'c, 'd) poly = ('a, 'b, 'c, 'd) P.t *)

(* ****************************************************************************)
(** {2 Predefined functors for polymorphic types} *)
(* ****************************************************************************)

(* module Poly_pair: Polymorphic2 with type ('a, 'b) poly = 'a * 'b *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(* module Pair(T1: S)(T2: S): S with type t = T1.t * T2.t *)
(* module Pair_with_collections(T1: S)(T2: S): *)
(*   S_with_collections with type t = T1.t * T2.t *)
(* val pair: 'a Type.t -> 'b Type.t -> ('a * 'b) Type.t *)

(* module Poly_ref: Polymorphic with type 'a poly = 'a ref *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(* module Ref(T: S) : S with type t = T.t ref *)
(* val t_ref: 'a Type.t -> 'a ref Type.t *)

(* module Poly_option: Polymorphic with type 'a poly = 'a option *)
(* module Option(T: S) : S with type t = T.t option *)

(** @since Nitrogen-20111001 *)
(* module Option_with_collections(T:S): *)
(*   S_with_collections with type t = T.t option *)

(* val option: 'a Type.t -> 'a option Type.t *)

(* module Poly_list: Polymorphic with type 'a poly = 'a list *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(* module List(T: S) : S with type t = T.t list *)

(* module List_with_collections(T:S): *)
(*   S_with_collections with type t = T.t list *)
(** @since Fluorine-20130401 *)

(* val list: 'a Type.t -> 'a list Type.t *)
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* module Poly_array: Polymorphic with type 'a poly = 'a array *)
(** @since Neon-20140301 *)

(* module Array(T: S) : S with type t = T.t array *)
(** @since Neon-20140301 *)

(* module Array_with_collections(T:S): *)
(*   S_with_collections with type t = T.t array *)
(** @since Neon-20140301 *)

(* val array: 'a Type.t -> 'a array Type.t *)
(** @since Neon-20140301 *)


(* module Poly_queue: Polymorphic with type 'a poly = 'a Queue.t *)
(* val queue: 'a Type.t -> 'a Queue.t Type.t *)
(* module Queue(T: S) : S with type t = T.t Queue.t *)

(* module Triple(T1: S)(T2: S)(T3: S): S with type t = T1.t * T2.t * T3.t *)
(* val triple: 'a Type.t -> 'b Type.t -> 'c Type.t -> ('a * 'b * 'c) Type.t *)
(** @since Fluorine-20130401 *)

(* module Triple_with_collections(T1: S)(T2: S)(T3: S): *)
(*   S_with_collections with type t = T1.t * T2.t * T3.t *)

(** @since Nitrogen-20111001 *)
(* module Quadruple(T1: S)(T2: S)(T3: S)(T4:S): *)
(*   S with type t = T1.t * T2.t * T3.t * T4.t *)
(* val quadruple: *)
(*   'a Type.t -> 'b Type.t -> 'c Type.t -> 'd Type.t -> ('a * 'b * 'c * 'd) Type.t *)
(** @since Fluorine-20130401 *)

(** @since Nitrogen-20111001 *)
(* module Quadruple_with_collections *)
(*     (T1: S)(T2: S)(T3: S)(T4:S): *)
(*   S_with_collections with type t = T1.t * T2.t * T3.t * T4.t *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(*module Function *)
(*    (T1: sig include S val label: (string * (unit -> t) option) option end) *)
(*    (T2: S) *)
(*  : S with type t = T1.t -> T2.t *)

(*val func: *)
(*  ?label:string * (unit -> 'a) option -> 'a Type.t -> *)
(*  'b Type.t -> *)
(*  ('a -> 'b) Type.t *)
(*(1** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *1) *)

(*val optlabel_func: *)
(*  string -> (unit -> 'a) -> 'a Type.t -> 'b Type.t -> ('a -> 'b) Type.t *)
(*(1** [optlabel_func lab dft ty1 ty2] is equivalent to *)
(*    [func ~label:(lab, Some dft) ty1 ty2] *1) *)

(*val func2: *)
(*  ?label1:string * (unit -> 'a) option -> 'a Type.t -> *)
(*  ?label2:string * (unit -> 'b) option -> 'b Type.t -> *)
(*  'c Type.t -> *)
(*  ('a -> 'b -> 'c) Type.t *)
(*(1** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *1) *)

(*val func3: *)
(*  ?label1:string * (unit -> 'a) option -> 'a Type.t -> *)
(*  ?label2:string * (unit -> 'b) option -> 'b Type.t -> *)
(*  ?label3:string * (unit -> 'c) option -> 'c Type.t -> *)
(*  'd Type.t -> *)
(*  ('a -> 'b -> 'c -> 'd) Type.t *)
(*(1** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *1) *)

(*val func4: *)
(*  ?label1:string * (unit -> 'a) option -> 'a Type.t -> *)
(*  ?label2:string * (unit -> 'b) option -> 'b Type.t -> *)
(*  ?label3:string * (unit -> 'c) option -> 'c Type.t -> *)
(*  ?label4:string * (unit -> 'd) option -> 'd Type.t -> *)
(*  'e Type.t -> *)
(*  ('a -> 'b -> 'c -> 'd -> 'e) Type.t *)

module Set
    (S: Set.S)(E: S with type t = S.elt):
  Set with type t = S.t and type elt = E.t

module Map
    (M: Map.S)(Key: S with type t = M.key):
  Map with type 'a t = 'a M.t and type key = M.key and module Key = Key

module Hashtbl
    (H: Hashtbl_with_descr)(Key: S with type t = H.key):
  Hashtbl with type 'a t = 'a H.t and type key = H.key and module Key = Key

module type Sub_caml_weak_hashtbl = sig
  type data
  type t
  val create: int -> t
  val add: t -> data -> unit
end

(* module Caml_weak_hashtbl(D: S): sig *)
(*   include Weak.S with type t = Weak.Make(D).t and type data = D.t *)
(*   module Datatype: S with type t = t *)
(* end *)

(* module Weak(W: Sub_caml_weak_hashtbl)(_: S with type t = W.data) : *)
(*   S with type t = W.t *)
