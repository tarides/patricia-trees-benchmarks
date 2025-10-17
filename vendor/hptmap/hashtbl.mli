(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.Hashtbl} module.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)

include module type of Stdlib.Hashtbl

(** Extension of {!Stdlib.Hashtbl.S}. *)
module type S = sig
  include S

  (** Iter on the hashtbl, but respecting the order on keys induced
      by [cmp]. Use [Stdlib.compare] if [cmp] not given.

      If the table contains several bindings for the same key, they
      are passed to [f] in reverse order of introduction, that is,
      the most recent binding is passed first.
  *)
  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit


  (** Fold on the hashtbl, but respecting the order on keys induced
      by [cmp]. Use [Stdlib.compare] if [cmp] not given.

      If the table contains several bindings for the same key, they
      are passed to [f] in reverse order of introduction, that is,
      the most recent binding is passed first.
  *)
  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Iter on the hashtable, respecting the order on entries
      given by [cmp]. The table may contains several bindings for the
      same key.
  *)
  val iter_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> unit) -> 'a t -> unit

  (** Fold on the hashtable, respecting the order on entries
      given by [cmp]. The table may contains several bindings for the
      same key.
  *)
  val fold_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Iter on the hashtable, respecting the order on entries
      given by [cmp]. The relative order for entries whose values is
      equal according to cmp, is not specified.
  *)
  val iter_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> unit) -> 'a t -> unit

  (** Fold on the hashtable, respecting the order on entries
      given by [cmp]. The relative order for entries whose values is
      equal according to cmp, is not specified.
  *)
  val fold_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Same as {!Stdlib.Hashtbl.find} but returns [~default] if the key is not
      found.
      @before 32.0-Germanium Was named [find_def] and [~default] was not named
      and the last argument.
  *)
  val find_default: default:'a -> 'a t -> key  -> 'a

  val find_def: 'a t -> key -> 'a -> 'a
  [@@deprecated "Use Hashtbl.find_default instead."]
  [@@migrate { repl = fun h k default -> Rel.find_default ~default h k }]

  val memo: 'a t -> key -> (key -> 'a) -> 'a
  (** [memo tbl k f] returns the binding of [k] in [tbl]. If there is
      no binding, add the binding [f k] associated to [k] in [tbl] and return
      it.
      @since Chlorine-20180501
  *)

end

module Make(H: HashedType) : S with type key = H.t
