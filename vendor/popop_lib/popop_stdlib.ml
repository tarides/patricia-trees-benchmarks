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

module Map = Extmap
module XHashtbl = Exthtbl.Hashtbl

(* Set, Map, Hashtbl on structures with a unique tag *)

let sexp_of_t_of_pp pp x =
  Base.Sexp.Atom (Fmt.str "%a" pp x)

module type TaggedType =
sig
  type t
  val tag : t -> int
  val pp: t Pp.pp
end

module type OrderedHashedType =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp: t Pp.pp
  val hash_fold_t: t Base.Hash.folder
end

module OrderedHashed (X : TaggedType) =
struct
  type t = X.t
  let hash = X.tag
  let [@ocaml.always inline] equal ts1 ts2 = X.tag ts1 == X.tag ts2 (** Todo ts1 == ts2? *)
  let compare ts1 ts2 = Stdlib.compare (X.tag ts1) (X.tag ts2)
  let pp = X.pp
  let hash_fold_t s x = Base.Hash.fold_int s (X.tag x)
end

module MakeMSH (X : TaggedType) =
struct
  module T = OrderedHashed(X)
  include T
  let hash_fold_t state t = Base.Hash.fold_int state (X.tag t)
  let sexp_of_t = sexp_of_t_of_pp pp
  module MGen = Intmap.Make(struct
      include X
      let equal ts1 ts2 = X.tag ts1 == X.tag ts2
    end)
  module M = MGen.NT
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(T)
end

module type Datatype = sig
  include OrderedHashedType
  val hash_fold_t: Base.Hash.state -> t -> Base.Hash.state
  module M : Map_intf.PMap with type key = t
  module S : Map_intf.Set with type 'a M.t = 'a M.t
                           and type M.key = M.key
  module H : Exthtbl.Hashtbl.S with type key = t
  include Base.Hashtbl.Key.S with type t := t
end

module type NamedDatatype = sig
  include Datatype

  val name : string
end

module type Printable = sig
  include OrderedHashedType
  val pp: t Pp.pp
end

module MkDatatype(T : OrderedHashedType) = struct
  let hash_fold_t state t = Base.Hash.fold_int state (T.hash t)
  let sexp_of_t = sexp_of_t_of_pp T.pp
  module M = Mergemap.Make(T)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(T)
end

(* Set, Map, Hashtbl on ints and strings *)

module Int = struct
  type t = int
  let equal (x : int) y = x = y
  let hash  (x : int) = x
  let tag x = x
  let pp = Pp.int
 end


module DInt = struct
  include Base.Int
  let pp fmt x = Format.pp_print_int fmt x
  let hash_fold_t = Base.Int.hash_fold_t
  module GM  = Intmap.Make(Int)
  module M = GM.NT
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Int)
end

module DIntOrd = DInt

module DUnit = struct
  include Unit
  let sexp_of_t = Base.Unit.sexp_of_t
end

module DBool = struct
  include Base.Bool
  module M = Map.Make(Base.Bool)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Base.Bool)
end

module DStr = struct
  include Base.String
  let hash_fold_t = Base.String.hash_fold_t
  module M = Map.Make(Base.String)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Base.String)
end


module DFloat = struct
  include Base.Float
  let hash_fold_t = Base.Float.hash_fold_t
  module M = Map.Make(Base.Float)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Base.Float)
end

