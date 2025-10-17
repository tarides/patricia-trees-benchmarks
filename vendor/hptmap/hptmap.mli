(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier QPL-1.0-INRIA-2004-exception                   *)
(*  This file was originally part of Menhir                                *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt             *)
(*  Copyright (C)                                                          *)
(*  Institut National de Recherche en Informatique et en Automatique.      *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives).  *)
(*                                                                         *)
(***************************************************************************)

(** Efficient maps from hash-consed trees to values, implemented as
    Patricia trees. *)

(** This implementation of big-endian Patricia trees follows Chris
    Okasaki's paper at the 1998 ML Workshop in Baltimore.  Maps are
    implemented on top of Patricia trees. A tree is big-endian if it
    expects the key's most significant bits to be tested first. *)

(** Undocumented. Needed for advanced users only *)
type prefix

(** Type of the keys of the map. *)
module type Id_Datatype = sig
  include Datatype.S
  val id: t -> int (** Identity of a key. Must verify [id k >= 0] and
                       [equal k1 k2 <==> id k1 = id k2] *)
end

(** This functor builds {!Hptmap_sig.Shape} for maps indexed by keys [Key],
    which contains all functions on hptmap that do not create or modify maps. *)
module Shape (Key : Id_Datatype): sig
  include Hptmap_sig.Shape with type key = Key.t
  type 'a t = 'a map
end

(** Required information for the correctness of the hptmaps. *)
module type Info = sig
  type key
  type v

  val initial_values : (key * v) list list
  (** List of the maps that must be shared between all instances of Frama-C
      (the maps being described by the list of their bindings).
      Must include all maps that are exported at Caml link-time when the
      functor is applied. *)

  val dependencies : State.t list
  (** Dependencies of the hash-consing table. The table will be cleared
      whenever one of those dependencies is cleared. *)
end

(** This functor builds the complete module of maps indexed by keys [Key]
    to values [V]. *)
module Make
    (Key : Id_Datatype)
    (V : Datatype.S)
    (_ : Info with type key := Key.t
               and type v := V.t)
  : Hptmap_sig.S with type key = Key.t
                  and type v = V.t
                  and type 'v map = 'v Shape(Key).map
                  and type prefix = prefix

(** An additional boolean information is computed for each tree, by composing
    the boolean on the subtrees and the value information on each leaf. *)
(*module type Compositional_bool = sig *)
(*  type key *)
(*  type v *)

(*  val empty : bool *)
(*  (1** Value for the empty tree *1) *)

(*  val leaf : key -> v -> bool *)
(*  (1** Value for a leaf *1) *)

(*  val compose : bool -> bool -> bool *)
(*  (1** Composition of the values of two subtrees *1) *)
(*end *)

(*(1** This functor builds the complete module of maps indexed by keys [Key] *)
(*    to values [V], with an additional boolean information maintained for *)
(*    each tree. *1) *)
(*module Make_with_compositional_bool *)
(*    (Key : Id_Datatype) *)
(*    (V : Datatype.S) *)
(*    (_ : Compositional_bool with type key := Key.t *)
(*                             and type v := V.t) *)
(*    (_ : Info with type key := Key.t *)
(*               and type v := V.t) *)
(*  : Hptmap_sig.S with type key = Key.t *)
(*                  and type v = V.t *)
(*                  and type 'v map = 'v Shape(Key).map *)
(*                  and type prefix = prefix *)
