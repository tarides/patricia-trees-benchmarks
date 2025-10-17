(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Type value. A type value is a value representing a static ML monomorphic
    type. This API is quite low level. Prefer to use module {!Datatype} instead
    whenever possible.

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ****************************************************************************)
(** {2 Type declaration} *)
(* ****************************************************************************)

type 'a t
(** Type of type values. For each monomorphic type [ty], a value of type [ty
    t] dynamically represents the type [ty]. Such a value is called a type
    value and should be unique for each static monomorphic type.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

type 'a ty = 'a t

(* ****************************************************************************)
(** {2 Pretty printing materials} *)
(* ****************************************************************************)

(** [par_ty_name f ty] puts parenthesis around the name of the [ty] iff [f ty]
    is [true].
    @since Carbon-20101201 *)
val par_ty_name: ('a t -> bool) -> 'a t -> string

(* ****************************************************************************)
(** {2 Constructor and getters} *)
(* ****************************************************************************)

(* exception AlreadyExists of string *)
(** May be raised by {!register}.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* val register: *)
(*   ?closure:bool -> *)
(*   name:string -> *)
(*   Structural_descr.t -> *)
(*   'a list -> *)
(*   'a t *)
(** [register ?closure ~name ~ml_name descr reprs] registers
    a new type value. Should not be used directly. Use one of functors of
    module {!Datatype} instead.
    [closure] is true iff the type is a function type.
    [name] is the name of the type. Must be a valid OCaml type name (eventually
    prefixed by a module path).
    @raise AlreadyExists if the given name is already used by another type.
    @raise Invalid_argument if [reprs] is the empty list
*)

exception No_abstract_type of string

(** Apply this functor to access to the abstract type of the given name.
    @raise No_abstract_type if no such abstract type was registered.
    @since Nitrogen-20111001
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Abstract(_: sig val name: string end): sig
  type t
  val ty: t ty
end

val name: 'a t -> string
(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* val structural_descr: 'a t -> Structural_descr.t *)
val reprs: 'a t -> 'a list
(** Not usable in the "no-obj" mode *)

val digest: 'a t -> Digest.t

val get_embedded_type_names: 'a t -> string list
(** Get the list of names containing in the type represented by the given type
    value. For instance [get_embedded_type_names (Datatype.func Datatype.unit
    (Datatype.list Datatype.int))] returns [ "unit -> int list"; "unit"; "int
    list"; "int" ].
    @since Oxygen-20120901 *)

val set_name: 'a t -> string -> unit
(** @since Neon-20140301 *)

(* ****************************************************************************)
(** {2 Type values are comparable} *)
(* ****************************************************************************)

val equal: 'a t -> 'b t -> bool
val compare: 'a t -> 'b t -> int
val hash: 'a t -> int

(* ****************************************************************************)
(** {2 Polymorphic type values}

    Functors for handling polymorphic type: one type value must be registered
    for each monomorphic instance of a polymorphic type. *)
(* ****************************************************************************)

module type Polymorphic_input = sig

  val name: 'a t -> string
  (** How to build a name for each monomorphic instance of the type
      value from the underlying type. *)

  (* val structural_descr: Structural_descr.t -> Structural_descr.t *)
  (** How to build the structural descriptor for each monomorphic instance.
      @since Carbon-20101201 *)

  type 'a t
  (** Static polymorphic type corresponding to its dynamic counterpart to
      register. *)

  (* val reprs: 'a -> 'a t list *)
  (** How to make the representant of each monomorphic instance of the
      polymorphic type value from an underlying representant. *)

end

(** For a polymorphic type value with one type variable, you must use an
    implementation of this signature. *)
module type Polymorphic = sig

  type 'a poly
  (** Type of the polymorphic type (for instance ['a list]). It must be
      instantiated before used. See function [instantiate] below. *)

  (* val instantiate: 'a t -> 'a poly t * bool *)
  (** @return the monomorphic instantiation of the polymorph type with the
      given type value. For instance, if ['a poly = 'a list], then
      [instantiate int] returns the type value [int list]. *)

  val is_instance_of: 'a t -> bool
  (** @return [true] iff the given type value has been created from
      function [instantiate] above.
      For instance, [is_instance_of (instantiate int)] always returns [true]
      but [is_instance_of int] always returns [false]. *)

  val get_instance: 'a poly t -> 'a t
  (** [get_instance ty] returns the type value used to create the given
      monomorphic instantiation. *)

end

(** Generic implementation of polymorphic type value. *)
module Polymorphic(T:Polymorphic_input)
  : Polymorphic with type 'a poly = 'a T.t

(** See module {!Polymorphic_input}: very same functions with one additional
    argument corresponding to the second type variable. *)
(* module type Polymorphic2_input = sig *)
(*   val name: 'a t -> 'b t -> string *)
(*   (1* val structural_descr: *1) *)
(*   (1*   Structural_descr.t -> Structural_descr.t -> Structural_descr.t *1) *)
(*   type ('a, 'b) t *)
(*   val reprs: 'a -> 'b -> ('a, 'b) t list *)
(* end *)

(** Same as {!module:Polymorphic} for polymorphic types with two type
    variables. *)
(* module type Polymorphic2 = sig *)
(*   type ('a, 'b) poly *)
(*   val instantiate: 'a t -> 'b t -> ('a, 'b) poly t * bool *)
(*   val is_instance_of: 'a t -> bool *)
(*   val get_instance: ('a, 'b) poly t -> 'a t * 'b t *)
(* end *)

(** Generic implementation of polymorphic type value with two type variables. *)
(* module Polymorphic2(T:Polymorphic2_input) *)
(*   : Polymorphic2 with type ('a, 'b) poly = ('a, 'b) T.t *)

(** Instance of {!module:Polymorphic2} for functions: same signature than
    {!module:Polymorphic2} with possibility to specify a label for the function
    parameter. *)
(*module Function : sig *)
(*  type ('a, 'b) poly = 'a -> 'b *)
(*  val instantiate: *)
(*    ?label:(string * (unit -> 'a) option) -> 'a t -> 'b t -> ('a -> 'b) t * bool *)
(*  (1** Possibility to add a label for the parameter. *)
(*      - [~label:(p,None)] for a mandatory labeled parameter [p]; *)
(*      - [~label:(p,Some f)] for an optional labeled parameter [p], *)
(*         with default value [f ()]. *1) *)

(*  val is_instance_of: 'a t -> bool *)
(*  val get_instance: ('a -> 'b) t -> 'a t * 'b t * string option *)
(*  val get_optional_argument: ('a -> 'b) t ->  (unit -> 'a) option *)
(*end *)

(** See module {!Polymorphic_input}: very same functions with two additional
    arguments corresponding to the second and third type variables.
    @since Oxygen-20120901 *)
(* module type Polymorphic3_input = sig *)
(*   val name: 'a t -> 'b t -> 'c t -> string *)
(*   (1* val structural_descr: *1) *)
(*   (1*   Structural_descr.t -> Structural_descr.t -> Structural_descr.t -> *1) *)
(*   (1*   Structural_descr.t *1) *)
(*   type ('a, 'b, 'c) t *)
(*   val reprs: 'a -> 'b -> 'c -> ('a, 'b, 'c) t list *)
(* end *)

(** Same as {!module:Polymorphic} for polymorphic types with three type
    variables.
    @since Oxygen-20120901 *)
(* module type Polymorphic3 = sig *)
(*   type ('a, 'b, 'c) poly *)
(*   val instantiate: 'a t -> 'b t -> 'c t -> ('a, 'b, 'c) poly t * bool *)
(*   val is_instance_of: 'a t -> bool *)
(*   val get_instance: ('a, 'b, 'c) poly t -> 'a t * 'b t * 'c t *)
(* end *)

(** Generic implementation of polymorphic type value with three type
    variables.
    @since Oxygen-20120901 *)
(* module Polymorphic3(T:Polymorphic3_input) *)
(*   : Polymorphic3 with type ('a, 'b, 'c) poly = ('a, 'b, 'c) T.t *)

(** See module {!Polymorphic_input}: very same functions with three additional
    arguments corresponding to the additional type variables.
    @since Oxygen-20120901 *)
(* module type Polymorphic4_input = sig *)
(*   val name: 'a t -> 'b t -> 'c t -> 'd t -> string *)
(*   (1* val structural_descr: *1) *)
(*   (1*   Structural_descr.t -> Structural_descr.t -> Structural_descr.t -> *1) *)
(*   (1*   Structural_descr.t -> Structural_descr.t *1) *)
(*   type ('a, 'b, 'c, 'd) t *)
(*   val reprs: 'a -> 'b -> 'c -> 'd -> ('a, 'b, 'c, 'd) t list *)
(* end *)

(** Same as {!module:Polymorphic} for polymorphic types with four type
    variables.
    @since Oxygen-20120901 *)
(* module type Polymorphic4 = sig *)
(*   type ('a, 'b, 'c, 'd) poly *)
(*   val instantiate: *)
(*     'a t -> 'b t -> 'c t -> 'd t -> ('a, 'b, 'c, 'd) poly t * bool *)
(*   val is_instance_of: 'a t -> bool *)
(*   val get_instance: ('a, 'b, 'c, 'd) poly t -> 'a t * 'b t * 'c t * 'd t *)
(* end *)

(** Generic implementation of polymorphic type value with four type
    variables.
    @since Oxygen-20120901 *)
(* module Polymorphic4(T:Polymorphic4_input) *)
(*   : Polymorphic4 with type ('a, 'b, 'c, 'd) poly = ('a, 'b, 'c, 'd) T.t *)

(* ****************************************************************************)
(** {2 Heterogeneous Tables}

    These tables are safe to use but nevertheless not for casual users. *)
(* ****************************************************************************)

(** @since Carbon-20101201 *)
(*module type Heterogeneous_table = sig *)

(*  type key *)
(*  (1** @since Carbon-20101201 *1) *)

(*  type 'a info *)
(*  type t *)
(*  (1** Type of heterogeneous (hash)tables indexed by values of type Key.t. *)
(*      Type values ensure type safety. *1) *)

(*  val create: int -> t *)
(*  (1** [create n] creates a new table of initial size [n]. *1) *)

(*  val add: t -> key -> 'a ty -> 'a info -> unit *)
(*  (1** [add tbl s ty v] binds [s] to the value [v] in the table [tbl]. *)
(*      If the returned value is a closure whose the type of one of its *)
(*      argument was dynamically registered, then it may raise *)
(*      [Incompatible_type]. *)
(*      @raise AlreadyExists if [s] is already bound in [tbl]. *)
(*  *1) *)

(*  exception Unbound_value of string *)
(*  exception Incompatible_type of string *)

(*  val find: t -> key -> 'a ty -> 'a info *)
(*  (1** [find tbl s ty] returns the binding of [s] in the table [tbl]. *)
(*      @raise Unbound_value if [s] is not bound in [tbl]. *)
(*      @raise Incompatible_type if [ty] was not the type value used to add *)
(*      the binding of [s] in [tbl]. *1) *)

(*  val iter: (key -> 'a ty -> 'a info -> unit) -> t -> unit *)
(*  (1** @since Oxygen-20120901 *1) *)

(*  val fold: (key -> 'a ty -> 'a info -> 'b -> 'b) -> t -> 'b -> 'b *)
(*  (1** @since Fluorine-20130401 *1) *)

(*end *)

(** Build an heterogeneous table associating keys to info.
    Not efficient for types registered without ml name.
    @since Carbon-20101201 *)
(* module Make_tbl *)
(*     (Key: sig include Hashtbl.HashedType val to_string: t -> string end) *)
(*     (Info: sig type 'a t end) : *)
(*   Heterogeneous_table with type key = Key.t and type 'a info = 'a Info.t *)

(** Heterogeneous tables indexed by string. *)
(* module String_tbl(Info: sig type 'a t end) *)
(*   : Heterogeneous_table with type key = string and type 'a info = 'a Info.t *)

(** Heterogeneous tables indexed by type value.
    Roughly the same signature that [Hashtbl.S]. *)
(* module Ty_tbl(Info: sig type 'a t end) : sig *)
(*   type t *)
(*   val create: int -> t *)
(*   val add: t -> 'b ty -> 'b Info.t -> unit *)
(*   val find: t -> 'b ty -> 'b Info.t *)
(* end *)

(** Heterogeneous table for the keys, but polymorphic for the values. *)
(* module Obj_tbl: sig *)
(*   type 'a t *)
(*   val create: unit -> 'a t *)
(*   val add: 'a t -> 'b ty -> 'b -> 'a -> unit *)
(*   val find: 'a t -> 'b ty -> 'b -> 'a *)
(*   val mem: 'a t -> 'b ty -> 'b -> bool *)
(*   val iter: 'b t -> ('a ty -> 'a -> 'b -> unit) -> unit *)
(* end *)

(**/**)
(* ****************************************************************************)
(** {2 Internal API} *)
(* ****************************************************************************)

val add_abstract_types: (string -> string -> unit) ref
