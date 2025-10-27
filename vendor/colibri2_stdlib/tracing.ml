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

module Trace = Trace_core

(* re-raise exception with its backtrace *)
external reraise : exn -> 'a = "%reraise"

(* trace_id metadata key *)
let trace_id_key : Int64.t Trace_core.Meta_map.key =
  Trace_core.Meta_map.Key.create ()

(* trace id counter *)
let trace_id : Int64.t ref = ref Int64.zero

let fresh_id () =
  (trace_id := Int64.(!trace_id + one));
  !trace_id

let get_id (os : Trace.explicit_span option) =
  match os with
  | None -> Int64.zero
  | Some s -> Trace_core.Meta_map.find_exn trace_id_key s.meta

let set_trace_id (span : Trace.explicit_span) id =
  span.meta <- Trace_core.Meta_map.add trace_id_key id span.meta

let collector out : Trace_core.collector =
  (* wrapped collector module from Trace_tef *)
  let module T = (val Trace_tef.collector ~out () : Trace.Collector.S) in
  (* our wrapper collector *)
  let module M = struct
    (* stack of opened explicit spans *)
    let spans : Trace.explicit_span list ref = ref []
    let peek_span () = match !spans with [] -> None | span :: _ -> Some span
    let push_span span = spans := span :: !spans

    let pop_span () =
      match !spans with
      | [] -> failwith "too many pops"
      | head :: tail ->
          spans := tail;
          head

    let shutdown () = T.shutdown ()
    let enter_span = T.enter_span
    let exit_span = T.exit_span
    let enter_manual_span = T.enter_manual_span
    let exit_manual_span = T.exit_manual_span
    let add_data_to_manual_span = T.add_data_to_manual_span
    let message = T.message
    let counter_float = T.counter_float
    let counter_int = T.counter_int
    let name_process = T.name_process
    let name_thread = T.name_thread

    (* override with_span so that the underlying collector emits
       manual enter/begin events, which ensures that the resulting
       trace is sorted by increasing time (compared to span entries
       which can only be generated on scope exit) *)

    let with_span ~__FUNCTION__:fun_name ~__FILE__:file ~__LINE__:line ~data
        name body =
      let more_data id parent_id (data : (string * Trace.user_data) list) =
        let kv k v = (k, `String (Int64.to_string v)) in
        kv "id" id :: kv "parent" parent_id :: data
      and parent = peek_span ()
      and id = fresh_id ()
      and cleanup () = exit_manual_span (pop_span ()) in
      let span : Trace.explicit_span =
        enter_manual_span ~parent ~__FUNCTION__:fun_name ~__FILE__:file
          ~__LINE__:line ~flavor:(Some `Sync)
          ~data:(more_data id (get_id parent) data)
          name
      in
      set_trace_id span id;
      push_span span;
      try
        let result = body span.span in
        cleanup ();
        result
      with exn ->
        cleanup ();
        reraise exn

    let add_data_to_span span data =
      match
        List.find_opt
          (fun (s : Trace.explicit_span) -> Stdlib.(s.span == span))
          !spans
      with
      | None -> failwith "add data failed to find span"
      | Some _ -> message "add_data_to_span" ~data ~span

    let extension_event _ = ()
    let _ = extension_event (* remove warning for tracing before 0.9 *)
  end in
  (module M)

let setup file = Trace_core.setup_collector @@ collector (`File file)

type pair = (string * Trace.user_data)

type 'a vformat =
  ('a, Format.formatter, unit, string) format4 -> 'a

type value_formatter = {
  fmt: 'a . 'a vformat
}

type Format.stag +=
  | Mark

let ostag parent tag =
  match tag with
  | Mark -> "\u{2}" (* start of text *)
  | _ -> parent tag

let cstag parent tag =
  match tag with
  | Mark -> "\u{3}" (* end of text *)
  | _ -> parent tag

(* same value as pp_infinity defined at version 5.2 *)
let infinity = 1000000010

let buffer = Buffer.create 1024
let clean = Str.global_replace (Str.regexp "[\n\r\t]+") ""
let strfmt =
  let ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin ppf infinity;
  Format.pp_set_mark_tags ppf true;
  let ostagfuns = Format.pp_get_formatter_stag_functions ppf ()
  in Format.pp_set_formatter_stag_functions ppf
    { ostagfuns with
      mark_open_stag = ostag ostagfuns.mark_open_stag;
      mark_close_stag = cstag ostagfuns.mark_close_stag };
  ppf

type message_args = value_formatter -> (pair list)

let fmt (type a) (fmt4:((a, Format.formatter, unit, string) format4)) : a =
  let ppf = strfmt in
  Buffer.clear buffer;
  let on_done ppf =
    Format.pp_print_flush ppf ();
    clean (Buffer.contents buffer)
    in Format.kfprintf on_done ppf fmt4

let emit (name:string) (args:message_args) : unit =
  (* add automatic additional fields here *)
  Trace.message name ~data:(fun () -> (args {fmt}))

(* this works in js console: *)
(* non-greedy match of text inside delimiters *)
(* "equality: \u0002@225\u0003\u0002@226\u0003".split(/\u{2}(.*?)\u{3}/u) *)

