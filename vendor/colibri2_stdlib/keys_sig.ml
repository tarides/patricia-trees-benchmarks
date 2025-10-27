(*************************************************************************)
(*  This file is part of Colibri2.                                       *)
(*                                                                       *)
(*  Copyright (C) 2017-2021                                              *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
(*    CNRS  (Centre national de la recherche scientifique)               *)
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

(** Key generators *)

(** Keys are the main programming tools used for implementing extensible types
    (sem, value, dom, pexp, ...) *)

open Std

(** {2 Exceptions} *)

(** {2 Type comparison and coercion} *)

module type Registry = sig
  type 'a key
  type 'a data

  val register : 'a data -> unit
  val check_is_registered : 'a key -> unit
  val is_well_initialized : unit -> bool
  val get : 'a key -> 'a data
  val print : 'a key -> 'a Format.printer

  type 'b iter_initialized = { iter : 'a. 'a data -> unit }

  val iter_initialized : unit iter_initialized -> unit

  type ('b, 'c) fold_initialized = { fold : 'a. 'c -> 'a data -> 'c }

  val fold_initialized : (unit, 'c) fold_initialized -> 'c -> 'c

  exception UnregisteredKey : 'a key -> exn
  (** the key shouldn't be used before its registration and shouldn't be
      registered again *)

  exception AlreadyRegisteredKey : 'a key -> exn
end

module type NamedType = sig
  type t

  val name : string
end

module type Key = sig
  (** Key with arity 1 *)

  (* module K: Datatype *)
  type 'a t

  val pp : 'a t Format.printer
  val compare : 'a t -> 'b t -> int
  val equal : 'a t -> 'b t -> bool
  val hash : 'a t -> int
  val tag : 'a t -> int
  val name : 'a t -> string

  type iter = { iter : 'a. 'a t -> unit } [@@unboxed]

  val iter : iter -> unit

  type 'b fold = { fold : 'a. 'a t -> 'b -> 'b } [@@unboxed]

  val fold : 'b fold -> 'b -> 'b

  module Eq : sig
    val eq_type : 'a t -> 'b t -> ('a, 'b) Poly.iseq
    (** If the two arguments are identical then an equality witness between the
        types is returned *)

    val coerce_type : 'a t -> 'b t -> ('a, 'b) Poly.eq
    (** If the two arguments are identical then an equality witness between the
        types is returned otherwise the exception BadCoercion is raised *)

    val coerce : 'a t -> 'b t -> 'a -> 'b
    (** If the two arguments are identical then covnert the argument otherwise
        taise BadCoercion *)
  end

  val create : (module NamedType with type t = 'a) -> 'a t

  module MkArray (D : sig
    type ('a, 'b) t
  end) :
    Array_hetero.S1
      with type 'a key = 'a t
       and type ('a, 'b) data = ('a, 'b) D.t

  module Array : Array_hetero.R1 with type 'a key = 'a t
  module ArrayH : Array_hetero.T1 with type 'a key = 'a t

  module MkHashtbl (D : sig
    type ('a, 'b) t
  end) :
    Hashtbl_hetero.S1
      with type 'a key = 'a t
       and type ('a, 'b) data = ('a, 'b) D.t

  module Hashtbl : Hashtbl_hetero.R1 with type 'a key = 'a t
  module HashtblH : Hashtbl_hetero.T1 with type 'a key = 'a t

  module MkMap (D : sig
    type ('a, 'b) t
  end) :
    Map_hetero.S with type 'a key = 'a t and type ('a, 'b) data = ('a, 'b) D.t

  module M : Map_hetero.R with type 'a key = 'a t

  module Make_Registry (S : sig
    type 'a data

    val pp : 'a data -> 'a Format.printer
    val key : 'a data -> 'a t
  end) : Registry with type 'a key := 'a t and type 'a data = 'a S.data
end

(* Arity 2 *)

module type NamedType2 = sig
  type t
  type d

  val name : string
end

module type Registry2 = sig
  type ('k, 'd) key
  type ('k, 'd) data

  val register : ('k, 'd) data -> unit
  val check_is_registered : ('k, 'd) key -> unit
  val is_well_initialized : unit -> bool
  val get : ('k, 'd) key -> ('k, 'd) data
  val printk : ('k, 'd) key -> 'k Format.printer
  val printd : ('k, 'd) key -> 'd Format.printer

  exception UnregisteredKey : ('a, 'b) key -> exn
  exception AlreadyRegisteredKey : ('a, 'b) key -> exn
end

module type Key2 = sig
  (** Key with arity 2 *)

  type ('k, 'd) t

  val pp : ('k, 'd) t Format.printer
  val equal : ('k1, 'd1) t -> ('k2, 'd2) t -> bool
  val hash : ('k, 'd) t -> int
  val name : ('k, 'd) t -> string

  type iter = { iter : 'k 'd. ('k, 'd) t -> unit } [@@unboxed]

  val iter : iter -> unit

  type 'b fold = { fold : 'a1 'a2. ('a1, 'a2) t -> 'b -> 'b } [@@unboxed]

  val fold : 'b fold -> 'b -> 'b

  val create :
    (module NamedType2 with type t = 'a1 and type d = 'a2) -> ('a1, 'a2) t

  module Eq : sig
    val eq_type :
      ('a1, 'a2) t -> ('b1, 'b2) t -> ('a1 * 'a2, 'b1 * 'b2) Poly.iseq
    (** If the two arguments are identical then an equality witness between the
        types is returned *)

    val coerce_type :
      ('a1, 'a2) t -> ('b1, 'b2) t -> ('a1 * 'a2, 'b1 * 'b2) Poly.eq
    (** If the two arguments are identical then an equality witness between the
        types is returned otherwise the exception BadCoercion is raised *)
  end

  module MkArray (D : sig
    type ('k, 'd, 'b) t
  end) :
    Array_hetero.S2
      with type ('k, 'd) key = ('k, 'd) t
       and type ('k, 'd, 'b) data = ('k, 'd, 'b) D.t

  module Array : Array_hetero.R2 with type ('a, 'b) key = ('a, 'b) t

  module MkHashtbl (D : sig
    type ('k, 'd, 'b) t
  end) :
    Hashtbl_hetero.S2
      with type ('k, 'd) key = ('k, 'd) t
       and type ('k, 'd, 'b) data = ('k, 'd, 'b) D.t

  module Make_Registry (S : sig
    type ('k, 'd) data

    val ppk : ('k, 'd) data -> 'k Format.printer
    val ppd : ('k, 'd) data -> 'd Format.printer
    val key : ('k, 'd) data -> ('k, 'd) t
  end) :
    Registry2
      with type ('k, 'd) key := ('k, 'd) t
       and type ('k, 'd) data = ('k, 'd) S.data
end
