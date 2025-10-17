(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** State builders.
    Provide ways to implement signature [State_builder.S].
    Depending on the builder, also provide some additional useful
    information.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************* *)
(* ************************************************************************* *)
(** {2 Low-level Builder} *)
(* ************************************************************************* *)
(* ************************************************************************* *)

(** Additional information required by {!State_builder.Register}. *)
module type Info = sig
  val name: string
  (** Name of the internal state. *)

  (* val dependencies : State.t list *)
  (** Dependencies of this internal state. *)
end

module type Info_with_size = sig
  include Info
  val size: int (** Initial size for the hash table. *)
end

(** Output signature of {!State_builder.Register}. *)
module type S = sig

  (* val self: State.t *)
  (** The kind of the registered state. *)

  (* val name: string *)

  (* val mark_as_computed: ?project:Project.t -> unit -> unit *)
  (** Indicate that the registered state will not change again for the
      given project (default is [current ()]). *)

  (* val is_computed: ?project:Project.t -> unit -> bool *)
  (** Returns [true] iff the registered state will not change again for the
      given project (default is [current ()]). *)

  (** Exportation of some inputs (easier use of [State_builder.Register]). *)

  module Datatype: Datatype.S

  (* val add_hook_on_update: (Datatype.t -> unit) -> unit *)
  (** Add an hook which is applied each time (just before) the project library
      changes the local value of the state.
      @since Nitrogen-20111001 *)

  (* val howto_marshal: (Datatype.t -> 'a) -> ('a -> Datatype.t) -> unit *)
  (** [howto_marshal marshal unmarshal] registers a custom couple of
      functions [(marshal, unmarshal)] to be used for serialization.
      Default functions are identities. In particular, this
      function must be used if [Datatype.t] is not marshallable and
      [do_not_save] is not called.
      @since Boron-20100401 *)

end

(** [Register(Datatype)(Local_state)(Info)] registers a new state.
    [Datatype] represents the datatype of a state, [Local_state]
    explains how to deal with the client-side state and [Info] are additional
    required information.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(* module Register *)
(*     (Datatype: Datatype.S) *)
(*     (_: State.Local with type t = Datatype.t) *)
(*     (_: sig include Info val unique_name: string end) *)
(*   : S with module Datatype = Datatype *)

(* ************************************************************************* *)
(* ************************************************************************* *)
(** {2 High-level Builders} *)
(* ************************************************************************* *)
(* ************************************************************************* *)

(* ************************************************************************* *)
(** {3 References} *)
(* ************************************************************************* *)

(** Output signature of [Ref]. *)
(*module type Ref = sig *)
(*  include S *)

(*  (1** Type of the referenced value. *1) *)
(*  type data *)

(*  val set: data -> unit *)
(*  (1** Change the referenced value. *1) *)

(*  val get: unit -> data *)
(*  (1** Get the referenced value. *1) *)

(*  val clear: unit -> unit *)
(*  (1** Reset the reference to its default value. *1) *)

(*  val add_hook_on_change: (data -> unit) -> unit *)
(*  (1** Add an hook which is applied each time (just after) the value of the state *)
(*      changes inside the current project. *)
(*      @since 28.0-Nickel *1) *)
(*end *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
(* module Ref *)
(*     (Data:Datatype.S) *)
(*     (_:sig *)
(*        include Info *)
(*        val default: unit -> Data.t *)
(*      end) *)
(*   : Ref with type data = Data.t and type Datatype.t = Data.t ref *)

(** Output signature of [Option_ref]. Note that [get] will raise [Not_found]
    if the stored data is [None]. Use [get_option] if you want to have
    access to the option.
*)
(*module type Option_ref = sig *)
(*  include Ref *)
(*  val memo: ?change:(data -> data) -> (unit -> data) -> data *)
(*  (1** Memoization. Compute on need the stored value. *)
(*      If the data is already computed (i.e. is not [None]), *)
(*      it is possible to change with [change]. *1) *)

(*  val map: (data -> data) -> data option *)
(*  val may: (data -> unit) -> unit *)
(*  val get_option : unit -> data option *)
(*  (1** @since Beryllium-20090901 *1) *)

(*  val add_hook_on_change: (data option -> unit) -> unit *)
(*  (1** Add an hook which is applied each time (just after) the value of the state *)
(*      changes inside the current project. *)
(*      @since 28.0-Nickel *1) *)
(*end *)

(** Build a reference on an option. *)
(* module Option_ref(Data:Datatype.S)(_: Info) : *)
(*   Option_ref with type data = Data.t *)

(** Output signature of [ListRef].
    @since Boron-20100401 *)
(*module type List_ref = sig *)
(*  type data_in_list *)
(*  include Ref *)
(*  val add: data_in_list -> unit *)
(*  (1** @since Nitrogen-20111001 *1) *)

(*  val iter: (data_in_list -> unit) -> unit *)
(*  val fold_left: ('a -> data_in_list -> 'a) -> 'a -> 'a *)
(*end *)

(** Build a reference on a list.
    @since Boron-20100401 *)
(* module List_ref(Data:Datatype.S)(_: Info) : *)
(*   List_ref with type data = Data.t list and type data_in_list = Data.t *)

(** Build a reference on an integer.
    @since Carbon-20101201 *)
(* module Int_ref(_:sig include Info val default: unit -> int end) : *)
(*   Ref with type data = int *)

(** Build a reference on an integer, initialized with [0].
    @since Carbon-20101201 *)
(* module Zero_ref(_:Info) : Ref with type data = int *)

(** Build a reference on a boolean.
    @since Oxygen-20120901 *)
(* module Bool_ref(_:sig include Info val default: unit -> bool end) : *)
(*   Ref with type data = bool *)

(** Build a reference on a boolean, initialized with [false].
    @since Carbon-20101201 *)
(* module False_ref(_:Info): Ref with type data = bool *)

(** Build a reference on a boolean, initialized with [true].
    @since Carbon-20101201 *)
(* module True_ref(_:Info): Ref with type data = bool *)

(** Build a reference on a float.
    @since Oxygen-20120901 *)
(* module Float_ref(_:sig include Info val default: unit -> float end) : *)
  (* Ref with type data = float *)

(* ************************************************************************* *)
(** {3 Weak Hashtbl} *)
(* ************************************************************************* *)

(** Output signature of builders of hashtables.
    @since Boron-20100401 *)
module type Weak_hashtbl = sig

  include S
  (** Hashtbl are a standard computation.
      BUT it is INCORRECT to use projectified hashtables if keys have a
      custom [rehash] function (see {!Datatype.Make_input.rehash}) *)

  type data
  (** @since Boron-20100401 *)

  val merge: data -> data
  (** [merge x] returns an instance of [x] found in the table if any, or else
      adds [x] and return [x].
      @since Boron-20100401 *)

  val add: data -> unit
  (** [add x] adds [x] to the table. If there is already an instance of [x],
      it is unspecified which one will be returned by subsequent calls to
      [find] and [merge].
      @since Boron-20100401 *)

  val clear: unit -> unit
  (** Clear the table.
      @since Boron-20100401 *)

  val count: unit -> int
  (** Length of the table.
      @since Boron-20100401 *)

  val iter: (data -> unit) -> unit
  (** @since Boron-20100401 *)

  val fold: (data -> 'a -> 'a) -> 'a -> 'a
  (** @since Boron-20100401 *)

  val find: data -> data
  (** [find x] returns an instance of [x] found in table.
      @raise Not_found if there is no such element.
      @since Boron-20100401 *)

  val find_all: data -> data list
  (** [find_all x] returns a list of all the instances of [x] found in t.
      @since Boron-20100401 *)

  val mem: data -> bool
  (** [mem x] returns [true] if there is at least one instance of [x] in the
      table, [false] otherwise.
      @since Boron-20100401 *)

  val remove: data -> unit
  (** [remove x] removes from the table one instance of [x]. Does nothing if
      there is no instance of [x].
      @since Boron-20100401 *)

end

(** Build a weak hashtbl over a datatype [Data] from a reference implementation
    [W].
    @since Boron-20100401 *)
module Weak_hashtbl
    (W: Weak.S)(_: Datatype.S with type t = W.data)(_: Info_with_size) :
  Weak_hashtbl with type data = W.data

(** Build a weak hashtbl over a datatype [Data] by using [Weak.Make] provided
    by the OCaml standard library. Note that the table is not saved on disk.
    @since Boron-20100401 *)
module Caml_weak_hashtbl(Data: Datatype.S)(_: Info_with_size) :
  Weak_hashtbl with type data = Data.t

(** Signature for the creation of projectified hashconsing tables..
    @since Aluminium-20160501 *)
module type Hashconsing_tbl =
  functor
    (Data: sig
       (** The hashconsed datatype *)
       include Datatype.S

       val equal_internal: t -> t -> bool
       (** Equality on the datatype internally used by the built table. *)

       val hash_internal: t -> int
       (** Hash function for datatype internally used by the built table. *)

       val initial_values: t list
       (** Pre-existing values stored in the built table and shared by all
           existing projects. *)
     end) ->
  functor (_: Info_with_size) ->
    Weak_hashtbl with type data = Data.t

(** Weak hashtbl dedicated to hashconsing.
    Note that the resulting table is not saved on disk.
    @since Boron-20100401 *)
module Hashconsing_tbl_weak: Hashconsing_tbl

(** Hash table for hashconsing, but the internal table is _not_ weak
    (it is a regular hash table). This module should be used only in case
    perfect reproducibility matters, as the table will never be emptied by
    the GC.
    @since Aluminium-20160501  *)
module Hashconsing_tbl_not_weak: Hashconsing_tbl

(** Weak or non-weak hashconsing tables, depending on variable
    {!Cmdline.deterministic}.
    @since Aluminium-20160501  *)
module Hashconsing_tbl: Hashconsing_tbl


(* ************************************************************************* *)
(** {3 Hashtables}

    IMPORTANT: that is INCORRECT to use projectified hashtables if keys have a
    custom [rehash] function (see {!Datatype.Make_input.rehash}) *)
(* ************************************************************************* *)

(** Events emitted when an [Hashtbl] state changes. *)
type ('k,'v) hashtbl_event =
  | Update of 'k * 'v
  (** A binding in the hashtable has been added or modified. *)
  | Remove of 'k
  (** A binding in the hashtable has been removed. *)
  | Clear
  (** The hashtable has been cleared. *)

(** Output signature of builders of hashtables. *)
module type Hashtbl = sig
  include S
  (** Hashtbl are a standard computation.
      BUT that is INCORRECT to use projectified hashtables if keys have a
      custom [rehash] function (see {!Datatype.Make_input.rehash}) *)

  type key
  type data
  val replace: key -> data -> unit
  (** Add a new binding. The previous one is removed. *)

  val add: key -> data -> unit
  (** Add a new binding. The previous one is only hidden. *)

  val clear: unit -> unit
  (** Clear the table. *)

  val length: unit -> int
  (** Length of the table. *)

  val iter: (key -> data -> unit) -> unit
  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> data -> unit) -> unit
  val fold: (key -> data -> 'a -> 'a) -> 'a -> 'a
  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> data -> 'a -> 'a) -> 'a -> 'a
  val memo: ?change:(data -> data) -> (key -> data) -> key -> data
  (** Memoization. Compute on need the data associated to a given key using
      the given function.
      If the data is already computed, it is possible to change with
      [change]. *)

  val find: key -> data
  (** Return the current binding of the given key.
      @raise Not_found if the key is not in the table. *)

  val find_opt: key -> data option
  (** Return the current binding of the given key, or None if no such binding
      exists.
      @since 27.0-Cobalt *)

  val find_all: key -> data list
  (** Return the list of all data associated with the given key. *)

  val mem: key -> bool
  val remove: key -> unit

  val to_seq: unit -> (key * data) Seq.t
  (** Iterate on the whole table.
      @since 27.0-Cobalt *)

  val add_hook_on_change: ((key, data) hashtbl_event -> unit) -> unit
  (** Add an hook which is applied each time (just after) a (key,value) pair in
      the hashtable changes inside the current project.
      @since 28.0-Nickel *)
end

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    - [H] is the hashtable implementation
    - [Data] is the datatype for values stored in the table
*)
module Hashtbl
    (H: Datatype.Hashtbl)
    (Data: Datatype.S)
    (_: Info_with_size) :
  Hashtbl with type key = H.key and type data = Data.t
                                and module Datatype = H.Make(Data)

module Int_hashtbl(Data: Datatype.S)(_:Info_with_size):
  Hashtbl with type key = int and type data = Data.t

(* ************************************************************************* *)
(** {3 References on a set} *)
(* ************************************************************************* *)

(** Output signature of builders of references on a set. *)
(*module type Set_ref = sig *)
(*  include Ref *)
(*  type elt *)
(*  val add: elt -> unit *)
(*  val remove: elt -> unit *)
(*  (1** @since Neon-20140301 *1) *)

(*  val is_empty: unit -> bool *)
(*  val mem: elt -> bool *)
(*  val fold: (elt -> 'a -> 'a) -> 'a -> 'a *)
(*  val iter: (elt -> unit) -> unit *)
(*end *)

(*module Set_ref(S: Datatype.Set)(_: Info) *)
(*  : Set_ref with type elt = S.elt and type data = S.t *)

(* ************************************************************************* *)
(** {3 Queue} *)
(* ************************************************************************* *)

(* module type Queue = sig *)
(*   type elt *)
(*   (1* val self: State.t *1) *)
(*   val add: elt -> unit *)
(*   val iter: (elt -> unit) -> unit *)
(*   val fold: ('a -> elt -> 'a) -> 'a -> 'a *)
(*   val is_empty: unit -> bool *)
(*   val length: unit -> int *)
(* end *)

(* module Queue(Data: Datatype.S)(_: Info) : Queue with type elt = Data.t *)

(* ************************************************************************* *)
(** {3 Array} *)
(* ************************************************************************* *)

(* module type Array = sig *)
(*   type elt *)

(*   val length: unit -> int *)
(*   val set_length: int -> unit *)
(*   val get: int -> elt *)
(*   val set: int -> elt -> unit *)
(*   val iter : (elt -> unit) -> unit *)
(*   val iteri : (int -> elt -> unit) -> unit *)
(*   val fold_left: ('a -> elt -> 'a) -> 'a -> 'a *)
(*   val fold_right:  (elt -> 'a -> 'a) -> 'a -> 'a *)
(* end *)

(* module Array(Data: Datatype.S)(_: sig include Info val default: Data.t end) : *)
(*   Array with type elt = Data.t *)


(* ************************************************************************* *)
(** {3 Proxies} *)
(* ************************************************************************* *)

(** State proxy. A proxy is a state which does not correspond to any useful
    mutable value. Its goal is only to reduce the number of dependencies
    between groups of states.
    @since Carbon-20101201 *)
module Proxy : sig

  type t
  (** Proxy type. *)

  type kind =
    | Backward (** All states in the proxy depend on it. *)
    | Forward  (** The proxy depends on all states inside. *)
    | Both     (** States in the proxy and the proxy itself are mutually
                   dependent. *)

  val create: string -> kind -> State.t list -> t
  (** [create s k sk l] creates a new proxy with the given name, kinds and
      states inside it. *)

  val extend: State.t list -> t -> unit
  (** Add some states in the given proxy. *)

  val get: t -> State.t
  (** Getting the state corresponding to a proxy. *)

end

(* ************************************************************************* *)
(** {3 Counters} *)
(* ************************************************************************* *)

module type Counter = sig

  val next : unit -> int
  (** Increments the counter and returns a fresh value *)

  val get: unit -> int
  (** @return the current value of the counter, without incrementing it.
      @since Fluorine-20130401 *)

  (** Resets the counter to 0.
      @since 29.0-Copper *)
  val reset: unit -> unit

  (* val self: State.t *)
  (** @since Oxygen-20120901 *)

end

(** Creates a counter that is shared among all projects, but which is
    marshalling-compliant.
    @since Carbon-20101201 *)
(* module SharedCounter(_ : sig val name : string end) : Counter *)

(** Creates a projectified counter. That starts at 0

    @since Nitrogen-20111001 *)
(* module Counter(_ : sig val name : string end) : Counter *)


(* ****************************************************************************)
(** {2 Generic functor to hashcons an arbitrary type } *)
(* ****************************************************************************)

(** Output signature of [Hashcons] below. *)
module type Hashcons = sig
  type elt (** The type of the elements that are hash-consed *)

  include Datatype.S_with_collections (** hashconsed version of {!elt} *)

  val hashcons: elt -> t
  (** Injection as an hashconsed value. *)

  val get:      t -> elt
  (** Projection out of hashconsing. *)

  val id: t -> int
  (** Id of an hashconsed value. Unique:
      [id x = id y] is equivalent to equality on {!elt}. *)

  (* val self: State.t *)
end

(** Hashconsed version of an arbitrary datatype *)
module Hashcons
    (Data: Datatype.S)
    (_: sig
       include Info
       val initial_values: Data.t list
       (** List of values created at compile-time, that must be shared between
           all instances of Frama-C. *)
     end)
  : Hashcons with type elt = Data.t


(* ************************************************************************* *)
(** {3 Useful operations} *)
(* ************************************************************************* *)

(* val apply_once: *)
(*   string -> State.t list -> (unit -> unit) -> (unit -> unit) * State.t *)
(** [apply_once name dep f] returns a closure applying [f] only once and the
    state internally used. [name] and [dep] are respectively the name and
    the dependencies of the local state created by this function.  Should
    be used partially applied. If [f] raises an exception, then it is
    considered as not applied. *)

(** @since Fluorine-20130401 *)
(* module States: sig *)

  (* val iter: *)
    (* ?prj:Project.t -> (string -> 'a Type.t -> 'a -> bool -> unit) -> unit *)
  (** iterates a function [f] over all registered states.  Arguments of [f] are
      its name, its type value, its value for the given project
      ([Project.current ()] by default) and a boolean which indicates if it is
      already computed.
      @since Fluorine-20130401
  *)

  (* val fold: *)
    (* ?prj:Project.t -> *)
    (* (string -> 'a Type.t -> 'a -> bool -> 'acc -> 'acc) -> 'acc -> 'acc *)
  (** As iter, but for folding.
      @since Fluorine-20130401*)

  (* val find: *)
    (* ?prj:Project.t -> string -> 'a Type.t -> 'a * bool *)
    (** @return the value of a state given by its name (and if it is computed), in
        the given project ([Project.current ()] by default) *)

(* end *)
