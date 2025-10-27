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

type flag

module LogLevel : sig
  type t

  val name : t -> string
  val of_string : string -> t
  val rank : t -> int
end

val register_flag :
  desc:(unit, unit, unit, unit, unit, unit) format6 -> string -> flag
(** Register a new flag. It is allowed to register twice the same flag *)

val register_info_flag :
  desc:(unit, unit, unit, unit, unit, unit) format6 -> string -> flag
(** Register a new info flag. It is allowed to register twice the same flag.
    Info flags are set by --debug-all (see set_info_flags) and must not change
    the behaviour. *)

val list_flags :
  unit ->
  (string * flag * bool * (unit, unit, unit, unit, unit, unit) format6) list
(** List the known flags, the boolean indicate if it is an info flag *)

val lookup_flag : string -> flag
(** get the flag *)

val is_info_flag : string -> bool
(** test if the flag is an info flag; see set_info_flags *)

val flag_desc : string -> (unit, unit, unit, unit, unit, unit) format6
(** get the description of the flag *)

val set_flag : flag -> bool
(** Set the state of a flag to true; return true if the value changed *)

val set_flag_s : string -> unit
(** Lookup flag corresponding to string and set its state to true *)

val unset_flag : flag -> unit

val set_info_flags : bool -> unit
(** activate all flags registered as info flags *)

val test_flag : flag -> bool
(** Return the state of the flag *)

val test_noflag : flag -> bool

val set_debug_formatter : Format.formatter -> unit
(** Set the formatter used when printing debug material *)

val get_debug_formatter : unit -> Format.formatter
(** Get the formatter used when printing debug material *)

val real_dprintf :
  ?nobox:unit -> ?ct:unit -> ('a, Format.formatter, unit) format -> 'a

val dprintf0 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  (unit, Format.formatter, unit) format ->
  unit
(** Print only if the flag is set *)

val dprintf1 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ('a -> unit, Format.formatter, unit) format ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf2 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ('b -> 'a -> unit, Format.formatter, unit) format ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf3 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ('c -> 'b -> 'a -> unit, Format.formatter, unit) format ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf4 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ('d -> 'c -> 'b -> 'a -> unit, Format.formatter, unit) format ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf5 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ('e -> 'd -> 'c -> 'b -> 'a -> unit, Format.formatter, unit) format ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf6 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ('f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit, Format.formatter, unit) format ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf7 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
    Format.formatter,
    unit )
  format ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf8 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
    Format.formatter,
    unit )
  format ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf9 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
    Format.formatter,
    unit )
  format ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf10 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
    Format.formatter,
    unit )
  format ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf11 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'k -> 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
    Format.formatter,
    unit )
  format ->
  'k ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf12 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'l -> 'k -> 'j -> 'i -> 'h -> 'g -> 'f -> 'e -> 'd -> 'c -> 'b -> 'a -> unit,
    Format.formatter,
    unit )
  format ->
  'l ->
  'k ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf13 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'm ->
    'l ->
    'k ->
    'j ->
    'i ->
    'h ->
    'g ->
    'f ->
    'e ->
    'd ->
    'c ->
    'b ->
    'a ->
    unit,
    Format.formatter,
    unit )
  format ->
  'm ->
  'l ->
  'k ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf14 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'n ->
    'm ->
    'l ->
    'k ->
    'j ->
    'i ->
    'h ->
    'g ->
    'f ->
    'e ->
    'd ->
    'c ->
    'b ->
    'a ->
    unit,
    Format.formatter,
    unit )
  format ->
  'n ->
  'm ->
  'l ->
  'k ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintf15 :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'o ->
    'n ->
    'm ->
    'l ->
    'k ->
    'j ->
    'i ->
    'h ->
    'g ->
    'f ->
    'e ->
    'd ->
    'c ->
    'b ->
    'a ->
    unit,
    Format.formatter,
    unit )
  format ->
  'o ->
  'n ->
  'm ->
  'l ->
  'k ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  unit
(** Print only if the flag is set *)

val dprintfn :
  ?nobox:unit ->
  ?ct:unit ->
  flag ->
  ( 'o ->
    'n ->
    'm ->
    'l ->
    'k ->
    'j ->
    'i ->
    'h ->
    'g ->
    'f ->
    'e ->
    'd ->
    'c ->
    'b ->
    'a ->
    'z,
    Format.formatter,
    unit )
  format ->
  'o ->
  'n ->
  'm ->
  'l ->
  'k ->
  'j ->
  'i ->
  'h ->
  'g ->
  'f ->
  'e ->
  'd ->
  'c ->
  'b ->
  'a ->
  'z
(** Print only if the flag is set *)

(* val dprintf : flag -> ('a, Format.formatter, unit) format -> 'a *)
(** Print only if the flag is set *)

val noassert : bool
(** is -noassert set *)

(** Command line arguments *)
module Args : sig
  type spec = Arg.key * Arg.spec * Arg.doc

  val desc_debug_list : spec
  (** Option for printing the list of debug flags *)

  val option_list : unit -> bool
  (** Print the list of flags if requested (in this case return [true]). You
      should run this function after the plugins have been loaded. *)

  val desc_debug_all : spec
  (** Option for setting all info flags *)

  val desc_debug : spec list
  (** Option for specifying a debug flag to set *)

  val desc_shortcut : string -> Arg.key -> Arg.doc -> spec
  (** Option for setting a specific flag *)

  val set_flags_selected : unit -> unit
  (** Set the flags selected by debug_all, debug or a shortcut. You should run
      this function after the plugins have been loaded. *)
end

val stats : flag
(** Stats *)

type 'a stat

val register_stats : pp:'a Fmt.t -> name:string -> init:'a -> 'a stat
val modstats0 : 'a stat -> ('a -> 'a) -> unit
val modstats1 : 'a stat -> ('a -> 'b -> 'a) -> 'b -> unit
val modstats2 : 'a stat -> ('a -> 'b -> 'c -> 'a) -> 'b -> 'c -> unit
val register_stats_int : string -> int stat
val register_stats_time : string -> float stat

val register_stats_histo :
  string -> int Colibri2_popop_lib.Popop_stdlib.DInt.H.t stat

val incr : int stat -> unit
val decr : int stat -> unit
val max : int stat -> (unit -> int) -> unit
val get_stats : int stat -> int
val add_time_during : float stat -> (Trace_core.span -> 'a) -> 'a

val histo_incr :
  int Colibri2_popop_lib.Popop_stdlib.DInt.H.t stat -> int -> unit

val print_stats : unit -> unit

type log_options = {
  parent : string option;
  is_info : bool;
  level : string;
  prefix : string option;
}

val defaults : log_options

(** Signature of imput modules for [RegisterDebugFlag] functor *)
module type DebugFlag = sig
  (* Initial log level, unless modified by user *)
  val options : log_options

  (* tag of debug flag *)
  val tag : string

  (* human-friendly description *)
  val desc : (unit, unit, unit, unit, unit, unit) format6
end

(** Register a debug flag [--debug-flag=<tag>] with a given description [desc].
    The returned module exposes logger functions (e.g. [info], [debug], [warn])
    that behaves like [printf] but also emits to the logging output by adding a
    prefix to the line being emitted (e.g. ["[tag] ..."]). Additionally, each
    message is traced using a generic message "debug_flag" with the following
    arguments: "tag" (set to <tag>)), "level" to the debugging level, and
    "message", the message being logged (without prefix). *)
module RegisterDebugFlag (_ : DebugFlag) : sig
  module DF : DebugFlag

  val active : unit -> bool
  val active_at : LogLevel.t -> bool
  val flag : flag
  val log : 'a. LogLevel.t -> ('a, Format.formatter, unit) format -> 'a
  val debug : 'a. ('a, Format.formatter, unit) format -> 'a
  val info : 'a. ('a, Format.formatter, unit) format -> 'a
  val notice : 'a. ('a, Format.formatter, unit) format -> 'a
  val warn : 'a. ('a, Format.formatter, unit) format -> 'a
  val error : 'a. ('a, Format.formatter, unit) format -> 'a

  (* Force message even if the flag is not active; FIXME: remove calls
     to this function and remove from interface *)
  val force_info : ('a, Format.formatter, unit) format -> 'a
end
