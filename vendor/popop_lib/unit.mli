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

(** Map, Set, and hash for Unit type. Mainly for fun *)

type t = unit

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val hash_fold_t : t Base.Hash.folder
val pp : t Pp.pp

module M :
  Map_intf.Map
    with type key = unit
     and type +'a t = 'a option
     and type 'a data = 'a

module S : Map_intf.Set with module M = M
module H : Exthtbl.Hashtbl.S with type key = unit
