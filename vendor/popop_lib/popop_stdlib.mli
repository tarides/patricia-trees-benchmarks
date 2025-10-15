(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2017   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

module Map : module type of Extmap
module XHashtbl : Exthtbl.Hashtbl

(* Set, Map, Hashtbl on structures with a unique tag *)

val sexp_of_t_of_pp : 'a Fmt.t -> 'a -> Base.Sexp.t

module type TaggedType = sig
  type t

  val tag : t -> int
  val pp : t Pp.pp
end

module type OrderedHashedType = sig
  type t

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Pp.pp
  val hash_fold_t : t Base.Hash.folder
end

module type Datatype = sig
  include OrderedHashedType

  val hash_fold_t : t Base.Hash.folder

  module M : Map_intf.PMap with type key = t
  module S : Map_intf.Set with type 'a M.t = 'a M.t and type M.key = M.key
  module H : Exthtbl.Hashtbl.S with type key = t
  include Base.Hashtbl.Key.S with type t := t
end

module type NamedDatatype = sig
  include Datatype

  val name : string
end

module type Printable = sig
  include OrderedHashedType

  val pp : t Pp.pp
end

module MkDatatype (T : OrderedHashedType) : sig
  val hash_fold_t : T.t Base.Hash.folder

  module M : Map_intf.PMap with type key = T.t
  module S : Map_intf.Set with type 'a M.t = 'a M.t and type M.key = M.key
  module H : Exthtbl.Hashtbl.S with type key = T.t

  val sexp_of_t : T.t -> Base.Sexp.t
end

module OrderedHashed (X : TaggedType) : OrderedHashedType with type t = X.t
module MakeMSH (X : TaggedType) : Datatype with type t = X.t

(* Set, Map, Hashtbl on ints and strings *)

module DInt : Datatype with type t = int
module DIntOrd : Datatype with type t = int
module DStr : Datatype with type t = string
module DFloat : Datatype with type t = float
module DUnit : Datatype with type t = unit
module DBool : Datatype with type t = bool
