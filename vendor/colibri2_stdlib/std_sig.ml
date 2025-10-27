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

exception Impossible (* Absurd *)
exception TODO of string

module type TaggedType = sig
  type t

  val tag : t -> int
  val pp : t Format.printer
end

module type OrderedHashedType = sig
  type t

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Format.printer
end

(* module type Datatype = sig
 *   include OrderedHashedType
 *
 *   module M : Map_intf.PMap with type key = t
 *   module S : Map_intf.Set with type 'a M.t = 'a M.t
 *                            and type M.key = M.key
 *   module H : Exthtbl.Hashtbl.S with type key = t
 * end *)
