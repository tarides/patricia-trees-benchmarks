(*************************************************************************)
(*  This file is part of Colibri2.                                       *)
(*                                                                       *)
(*  Copyright (C) 2014-2024                                              *)
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

(* instantiate a new collector for the given output *)
val collector : Trace_tef.output -> Trace_core.collector

(* register collector to Trace *)
val setup : string -> unit

(* shorthand notation for the function that "%a" expects to find *)

type pair = (string * Trace_core.user_data)

type 'a vformat =
  ('a, Format.formatter, unit, string) format4 -> 'a

type value_formatter = {
  fmt: 'a . 'a vformat
}

type Format.stag +=
  | Mark

type message_args = value_formatter -> (pair list)

val fmt : (('a, Format.formatter, unit, string) format4) -> 'a

(* emit a trace message for a traceable value by providing a custom
   formatter and adding additional keys/values in args (additional
   keys are prefixed with "#" to have something like a distinct
   namespace). *)
val emit : string -> message_args -> unit
