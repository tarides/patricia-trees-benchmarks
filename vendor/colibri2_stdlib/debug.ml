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

module Trace = Trace_core

let noassert =
  let x = ref true in
  assert (
    x := false;
    true);
  !x

let formatter = ref Format.err_formatter
let () = Format.pp_set_margin !formatter 300

let () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         print_endline "Stopped by user";
         exit 1))

exception UnknownFlag of string
exception UnknownLogLevel of string

module LogLevel = struct
  type t =
    (* filter only: show all messages *)
    | All
    (* debugging information, may impact performance *)
    | Debug
    (* minimal amount of information *)
    | Info
    (* important results (default values for RegisterDebugFlags flags) *)
    | Notice
    (* unexpected state or results, not critical *)
    | Warning
    (* critical errors or bugs *)
    | Error
    (* filter only: block all messages *)
    | Quiet

  let name = function
    | Debug -> "debug"
    | Info -> "info"
    | Notice -> "notice"
    | Warning -> "Warning"
    | Error -> "ERROR"
    | Quiet -> failwith "invalid level (quiet)"
    | All -> failwith "invalid level (all)"

  let of_string s =
    match String.lowercase_ascii s with
    | "a" | "all" -> All
    | "d" | "debug" -> Debug
    | "i" | "info" -> Info
    | "n" | "notice" -> Notice
    | "w" | "warn" | "warning" -> Warning
    | "e" | "error" -> Error
    | "q" | "quiet" -> Quiet
    | s -> raise (UnknownLogLevel s)

  let rank = function
    | All -> -1000
    | Debug -> 0
    | Info -> 1
    | Notice -> 2
    | Warning -> 3
    | Error -> 4
    | Quiet -> 1000
end

type flag = LogLevel.t ref

let flag_table = Hashtbl.create 17
let fst3 (flag, _, _) = flag
let snd3 (_, info, _) = info
let thd3 (_, _, desc) = desc

let gen_register_flag (desc : (unit, unit, unit, unit, unit, unit) format6) s
    info =
  try fst3 (Hashtbl.find flag_table s)
  with Not_found ->
    let flag = ref LogLevel.Quiet in
    Hashtbl.replace flag_table s (flag, info, desc);
    flag

let register_info_flag ~desc s = gen_register_flag desc s true
let register_flag ~desc s = gen_register_flag desc s false

let list_flags () =
  Hashtbl.fold
    (fun s (v, info, desc) acc -> (s, v, info, desc) :: acc)
    flag_table []

let flag_level string =
  match String.split_on_char ':' string with
  | [ flag ] -> (flag, LogLevel.Info)
  | [ flag; level ] -> (flag, LogLevel.of_string level)
  | _ -> raise (UnknownFlag string)

let lookup_flag s =
  try fst3 (Hashtbl.find flag_table s) with Not_found -> raise (UnknownFlag s)

let is_info_flag s =
  try snd3 (Hashtbl.find flag_table s) with Not_found -> raise (UnknownFlag s)

let flag_desc s =
  try thd3 (Hashtbl.find flag_table s) with Not_found -> raise (UnknownFlag s)

let test_flag_level s level =
  let flag_rank = LogLevel.rank !s and message_rank = LogLevel.rank level in
  message_rank >= flag_rank

let test_flag s = match !s with LogLevel.Quiet -> false | _ -> true
let test_noflag s = match !s with LogLevel.Quiet -> true | _ -> false

let set_flag s =
  let current = LogLevel.rank !s in
  if current > LogLevel.rank Info
  (* quiet forces the log to never be printed *)
  (* && current < (LogLevel.rank Quiet) *)
  then (
    s := LogLevel.Info;
    true)
  else false

let set_flag_s s =
  let s, level = flag_level s in
  lookup_flag s := level

let unset_flag s = s := LogLevel.Quiet

let set_info_flags debug =
  if debug then
    List.iter
      (fun (_, f, info, _) -> if info then ignore (set_flag f))
      (list_flags ())

let () =
  Printexc.register_printer (fun e ->
      match e with
      | UnknownFlag s -> Some (Fmt.str "unknown debug flag `%s'" s)
      | _ -> None)

let timestamp =
  register_flag "timestamp"
    ~desc:"Print@ a@ timestamp@ before@ debugging@ messages."

let time_start = Unix.gettimeofday ()

let set_debug_formatter fmt =
  (* enable the usual behavior of stderr: flush at every new line *)
  let out = Format.pp_get_formatter_out_functions fmt () in
  Format.pp_set_formatter_out_functions fmt
    {
      out with
      out_newline =
        (fun () ->
          out.out_newline ();
          out.out_flush ();
          if test_flag timestamp then
            let s =
              Printf.sprintf "<%f>" (Unix.gettimeofday () -. time_start)
            in
            out.out_string s 0 (String.length s));
    };
  Format.pp_open_vbox fmt 0;
  formatter := fmt

let get_debug_formatter () = !formatter
let () = set_debug_formatter Format.err_formatter

let real_dprintf ?nobox ?ct s =
  let box = match nobox with None -> true | Some () -> false in
  if box then (
    Format.pp_print_cut !formatter ();
    Format.pp_open_box !formatter 0);
  Format.kfprintf
    (fun fmt ->
      if box then (
        Format.pp_close_box fmt ();
        Format.pp_print_flush fmt ());
      if Option.is_some (ct : unit option) then
        let b = Base.Backtrace.get ~at_most_num_frames:7 () in
        let l =
          match Base.Backtrace.to_string_list b with _ :: _ :: l -> l | l -> l
        in
        Fmt.(
          pf !formatter "%a@." (vbox ~indent:1 (list ~sep:cut (box lines))) l))
    !formatter s
[@@inline never]

let test_info_level_flag s = test_flag_level s LogLevel.Info [@@inline always]

let dprintf0 ?nobox ?ct flag s =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s
[@@inline always]

let dprintf1 ?nobox ?ct flag s a1 =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s a1
[@@inline always]

let dprintf2 ?nobox ?ct flag s a1 a2 =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s a1 a2
[@@inline always]

let dprintf3 ?nobox ?ct flag s a1 a2 a3 =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s a1 a2 a3
[@@inline always]

let dprintf4 ?nobox ?ct flag s a1 a2 a3 a4 =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s a1 a2 a3 a4
[@@inline always]

let dprintf5 ?nobox ?ct flag s a1 a2 a3 a4 a5 =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5
[@@inline always]

let dprintf6 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 =
  if test_info_level_flag flag then real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6
[@@inline always]

let dprintf7 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7
[@@inline always]

let dprintf8 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8
[@@inline always]

let dprintf9 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9
[@@inline always]

let dprintf10 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
[@@inline always]

let dprintf11 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
[@@inline always]

let dprintf12 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
[@@inline always]

let dprintf13 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
[@@inline always]

let dprintf14 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
[@@inline always]

let dprintf15 ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
    a15 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
[@@inline always]

let dprintfn ?nobox ?ct flag s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
    a15 =
  if test_info_level_flag flag then
    real_dprintf ?nobox ?ct s a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
  else
    (* ifprintf take too many times for computing the format *)
    let rec aux _ = Obj.magic aux in
    Obj.magic aux

(*** Command-line arguments ****)

module Args = struct
  type spec = Arg.key * Arg.spec * Arg.doc

  let desc_debug_list, option_list =
    let opt_list_flags = ref false in
    let desc =
      ("--list-debug-flags", Arg.Set opt_list_flags, " List known debug flags")
    in
    let list () =
      (if !opt_list_flags then
         let list =
           Hashtbl.fold
             (fun s (_, info, desc) acc -> (s, info, desc) :: acc)
             flag_table []
         in
         let pp fmt (p, info, desc) =
           Format.fprintf fmt "@[%s%s  @[%( %)@]@]" p
             (if info then " *" else "")
             desc
         in
         Format.printf
           "@[<v 2>@[Known debug flags (`*' marks the flags selected by \
            --debug-all):@]@,\
            %a@]"
           Fmt.(list ~sep:(any "@\n") pp)
           (List.sort Stdlib.compare list));
      !opt_list_flags
    in
    (desc, list)

  let opt_list_flags = ref []
  let add_flag s = opt_list_flags := s :: !opt_list_flags

  let add_all_flags () =
    Hashtbl.iter (fun s (_, info, _) -> if info then add_flag s) flag_table

  let remove_flag s =
    opt_list_flags :=
      List.filter (fun x -> not (Base.phys_equal x s)) !opt_list_flags

  let desc_shortcut flag option desc =
    let set_flag () = add_flag flag in
    let desc = Fmt.str "%s (same as --debug %s)" desc flag in
    (option, Arg.Unit set_flag, desc)

  let desc_debug =
    [
      ("--debug", Arg.String add_flag, "<flag> Set a debug flag");
      ("--no-debug", Arg.String remove_flag, "<flag> Remove a debug flag");
    ]

  let desc_debug_all =
    let desc_debug =
      Fmt.str " Set all debug flags that do not change Why3 behaviour"
    in
    ("--debug-all", Arg.Unit add_all_flags, desc_debug)

  let set_flags_selected () =
    List.iter
      (fun flag ->
        let flag = lookup_flag flag in
        ignore (set_flag flag))
      !opt_list_flags
end

(** Stats *)
let stats = register_info_flag "stats" ~desc:"Compute and pp statistics."

type 'a stat = { mutable v : 'a; trace : string }

let max_name_size = ref 0

type stats = Stat : 'a Fmt.t * string * 'a stat -> stats

let registered_stats : stats list ref = ref []

let rec print_nb_char fmt = function
  | n when n <= 0 -> ()
  | n ->
      Format.pp_print_char fmt ' ';
      print_nb_char fmt (n - 1)

let print_stat fmt (Stat (pp, name, r)) =
  Format.fprintf fmt "@[%s%a: %a@]" name print_nb_char
    (!max_name_size - String.length name)
    pp r.v

let print_stats () =
  dprintf2 stats "@[%a@]@\n"
    Fmt.(list ~sep:(any "@.") print_stat)
    !registered_stats

let () =
  at_exit (fun () ->
      print_stats ();
      Format.pp_print_flush !formatter ())

(* SIGXCPU cpu time limit reached *)
let _ =
  (* TODO? have a possible callback for printing different message*)
  Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 2))

let register_stats ~pp ~name ~init =
  let s = { v = init; trace = name } in
  max_name_size := max !max_name_size (String.length name);
  registered_stats := Stat (pp, name, s) :: !registered_stats;
  s

let modstats0 r f = if test_flag stats then r.v <- f r.v
let modstats1 r f x = if test_flag stats then r.v <- f r.v x
let modstats2 r f x y = if test_flag stats then r.v <- f r.v x y

let register_stats_int name =
  register_stats ~pp:Format.pp_print_int ~name ~init:0

let register_stats_time name =
  register_stats ~pp:(fun fmt f -> Format.fprintf fmt "%.2fs" f) ~name ~init:0.

module HInt = Colibri2_popop_lib.Popop_stdlib.DInt.H

let register_stats_histo name =
  let pp fmt h =
    let l = HInt.fold (fun i v acc -> (i, v) :: acc) h [] in
    let l = List.sort (fun (a, _) (b, _) -> Int.compare a b) l in
    Fmt.list ~sep:Fmt.comma Fmt.(pair ~sep:(any "⇒") int int) fmt l
  in
  register_stats ~pp ~name ~init:(HInt.create 10)

let incr r =
  if test_flag stats then (
    r.v <- r.v + 1;
    Trace.counter_int r.trace r.v)

let decr r =
  if test_flag stats then (
    r.v <- r.v - 1;
    Trace.counter_int r.trace r.v)

let max r f =
  if test_flag stats then (
    let i = f () in
    r.v <- max r.v i;
    Trace.counter_int r.trace i)

let histo_incr r i =
  if test_flag stats then HInt.add_change (fun i -> i) ( + ) r.v i 1

let get_stats r : int = r.v

let add_time_during r f =
  let f () = Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ r.trace f in
  let gettime () = (Unix.times ()).tms_utime in
  if test_flag stats then
    let pre = gettime () in
    Fun.protect f ~finally:(fun () ->
        let post = gettime () in
        r.v <- r.v +. (post -. pre);
        Trace.counter_float r.trace r.v)
  else f ()

type log_options = {
  parent : string option;
  is_info : bool;
  level : string;
  prefix : string option;
}

let defaults : log_options =
  { parent = None; is_info = false; level = "notice"; prefix = None }

module type DebugFlag = sig
  val options : log_options
  val tag : string
  val desc : (unit, unit, unit, unit, unit, unit) format6
end

module RegisterDebugFlag (DF : DebugFlag) = struct
  module DF = DF

  let flag = gen_register_flag DF.desc DF.tag DF.options.is_info

  let () =
    (* non-"infos" debug flags may have side-effects that change the
       behavior of the program, so they are always quiet unless
       explicitly set *)
    if DF.options.is_info then flag := LogLevel.of_string DF.options.level

  (* TODO: log hierarchies (debug_all, debug_few, etc.) *)

  let active () = test_flag flag
  let active_at level = test_flag_level flag level
  let add_suffix = function LogLevel.Info -> false | _ -> true

  (* FIXME: filter emission of messages based on debug level *)
  let fmt_msg level s =
    let add_suffix = add_suffix !flag
    and prefix = Option.value DF.options.prefix ~default:DF.tag
    and level = LogLevel.name level in
    Tracing.emit "debug_flag" (fun _ -> [
            ("tag", `String DF.tag);
            ("level", `String level);
            ("message", `String s);
          ]);
    if add_suffix then real_dprintf "[%s:%s] %s" prefix level s
    else real_dprintf "[%s] %s" prefix s

  let log level =
   fun (type a) (fmt : (a, Format.formatter, unit, unit) format4) ->
    if test_flag_level flag level then Format.kasprintf (fmt_msg level) fmt
    else Format.ikfprintf (fun _ -> ()) !formatter fmt

  let debug f = log Debug f
  let info f = log Info f
  let notice f = log Notice f
  let warn f = log Warning f
  let error f = log Error f
  let force_info f = Format.kasprintf (fmt_msg Info) f
end
