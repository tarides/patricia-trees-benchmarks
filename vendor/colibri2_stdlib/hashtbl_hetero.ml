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

open Std
include Hashtbl_hetero_sig

module MakeS1
    (K : Keys1)
    (D : sig
      type ('a, 'b) t
    end) =
struct
  module Int = struct
    include Int

    let hash i = i
  end

  module H = Colibri2_popop_lib.Exthtbl.Hashtbl.Make (Int)

  type 'a key = 'a K.t
  type ('a, 'b) data = ('a, 'b) D.t
  type 'b elt = Pair : 'a key * ('a, 'b) D.t -> 'b elt
  type 'b t = 'b elt H.t
  type 'b iter_initialized = { iter : 'a. ('a, 'b) data -> unit }
  type 'b iter_initializedi = { iteri : 'a. 'a key -> ('a, 'b) data -> unit }
  type ('b, 'c) fold_initialized = { fold : 'a. 'c -> ('a, 'b) data -> 'c }

  type ('b, 'c) fold_initializedi = {
    foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c;
  }

  type printk = { printk : 'a. 'a key Format.printer }
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Format.printer }

  module Includable = struct
    let create capacity : 'b t = H.create capacity

    let get (type a) (m : 'b t) (k : a key) : (a, 'b) data =
      let (Pair (k', r)) = H.find m (K.tag k) in
      match K.equal k k' with Poly.Eq -> r | Poly.Neq -> raise IncoherentTable

    let get_def : 'b t -> 'a key -> ('a, 'b) data -> ('a, 'b) data =
     fun m k def -> try get m k with Not_found -> def

    let set (m : 'b t) (k : 'a key) (v : ('a, 'b) data) : unit =
      H.replace m (K.tag k) (Pair (k, v))

    let memo (type a) (memo : a key -> (a, 'b) data) (m : 'b t) (k : a key) :
        (a, 'b) data =
      match get m k with
      | exception Not_found ->
          let v = memo k in
          H.add m (K.tag k) (Pair (k, v));
          v
      | v -> v

    let is_uninitialized (m : 'b t) (k : 'a key) : bool =
      not (H.mem m (K.tag k))

    let remove (m : 'b t) (k : 'a key) : unit = H.remove m (K.tag k)
    let clear : 'b t -> unit = H.clear

    let iter_initialized (f : 'b iter_initialized) (m : 'b t) : unit =
      H.iter (fun _ (Pair (_, v)) -> f.iter v) m

    let fold_initialized (f : ('b, 'c) fold_initialized) (seed : 'c) (m : 'b t)
        =
      H.fold (fun _ (Pair (_, v)) sofar -> f.fold sofar v) m seed

    let iter_initializedi (f : 'b iter_initializedi) (m : 'b t) : unit =
      H.iter (fun _ (Pair (k, v)) -> f.iteri k v) m

    let fold_initializedi (f : ('b, 'c) fold_initializedi) (seed : 'c)
        (m : 'b t) : 'c =
      H.fold (fun _ (Pair (k, v)) sofar -> f.foldi sofar k v) m seed

    let copy : 'b t -> 'b t = H.copy

    let move ~from ~to_ =
      H.reset to_;
      let aux k v = H.replace to_ k v in
      H.iter aux from

    let pp (sep1 : unit Format.printer) (sep2 : unit Format.printer)
        (printkey : printk) (printdata : 'b printd) : 'b t Format.printer =
     fun fmt t ->
      let printkeydata fmt (Pair (k, v)) =
        Format.fprintf fmt "%a%a%a" printkey.printk k sep2 ()
          (printdata.printd k) v
      in
      let as_list = H.fold (fun _ v sofar -> v :: sofar) t [] in
      Format.list ~sep:sep1 printkeydata fmt as_list
  end

  include Includable
end

module MakeR1 (K : Keys1) : R1 with type 'a key = 'a K.t = struct
  include
    MakeS1
      (K)
      (struct
        type (_, 'b) t = 'b
      end)

  let iter_initialized f m = H.iter (fun _ (Pair (_, v)) -> f v) m

  let fold_initialized f seed m =
    H.fold (fun _ (Pair (_, v)) sofar -> f sofar v) m seed

  let apply_initialized f m =
    H.filter_mapi (fun _ (Pair (k, v)) -> Some (Pair (k, f v))) m

  let pp (type b) (sep1 : unit Format.printer) (sep2 : unit Format.printer)
      (printkey : printk) (printdata : b Format.printer) : b t Format.printer =
   fun fmt t ->
    let printkeydata fmt (Pair (k, v)) =
      Format.fprintf fmt "%a%a%a" printkey.printk k sep2 () printdata v
    in
    let as_list = H.fold (fun _ v sofar -> v :: sofar) t [] in
    Format.list ~sep:sep1 printkeydata fmt as_list
end

module MakeT1 (K : Keys1) : T1 with type 'a key = 'a K.t = struct
  module S1 =
    MakeS1
      (K)
      (struct
        type ('a, _) t = 'a
      end)

  type t = unit S1.t
  type 'a key = 'a K.t
  type iter_initialized = { iter : 'a. 'a -> unit }
  type iter_initializedi = { iteri : 'a. 'a key -> 'a -> unit }
  type 'c fold_initialized = { fold : 'a. 'c -> 'a -> 'c }
  type 'c fold_initializedi = { foldi : 'a. 'c -> 'a key -> 'a -> 'c }
  type printk = { printk : 'a. 'a key Containers.Format.printer }
  type printd = { printd : 'a. 'a key -> 'a Containers.Format.printer }

  include S1.Includable

  let iter_initialized { iter } = S1.iter_initialized { iter }
  let iter_initializedi { iteri } = S1.iter_initializedi { iteri }
  let fold_initialized { fold } = S1.fold_initialized { fold }
  let fold_initializedi { foldi } = S1.fold_initializedi { foldi }

  let pp (sep1 : unit Format.printer) (sep2 : unit Format.printer)
      (printkey : printk) (printdata : printd) : t Format.printer =
   fun fmt t ->
    let printkeydata fmt (S1.Pair (k, v)) =
      Format.fprintf fmt "%a%a%a" printkey.printk k sep2 () (printdata.printd k)
        v
    in
    let as_list = S1.H.fold (fun _ v sofar -> v :: sofar) t [] in
    Format.list ~sep:sep1 printkeydata fmt as_list
end

module MakeS2
    (K : Keys2)
    (D : sig
      type ('a1, 'a2, 'b) t
    end) :
  S2
    with type ('a1, 'a2) key = ('a1, 'a2) K.t
     and type ('a1, 'a2, 'b) data = ('a1, 'a2, 'b) D.t = struct
  module H = Hashtbl.Make (Int)

  type ('a1, 'a2) key = ('a1, 'a2) K.t
  type ('a1, 'a2, 'b) data = ('a1, 'a2, 'b) D.t
  type 'b elt = Pair : ('a1, 'a2) key * ('a1, 'a2, 'b) D.t -> 'b elt
  type 'b t = 'b elt H.t

  let create capacity : 'b t = H.create capacity

  let get (type a1 a2) (m : 'b t) (k : (a1, a2) key) : (a1, a2, 'b) data =
    let (Pair (k', r)) = H.find m (K.tag k) in
    match K.equal k k' with Poly.Eq -> r | Poly.Neq -> raise IncoherentTable

  let get_def :
      'b t -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> ('a1, 'a2, 'b) data =
   fun m k def -> try get m k with Not_found -> def

  let set (m : 'b t) (k : ('a1, 'a2) key) (v : ('a1, 'a2, 'b) data) : unit =
    H.replace m (K.tag k) (Pair (k, v))

  let memo memo m k =
    match get m k with
    | exception Not_found ->
        let v = memo k in
        H.add m (K.tag k) (Pair (k, v));
        v
    | v -> v

  let is_uninitialized (m : 'b t) (k : ('a1, 'a2) key) : bool =
    not (H.mem m (K.tag k))

  let remove (m : 'b t) (k : ('a1, 'a2) key) : unit = H.remove m (K.tag k)
  let clear : 'b t -> unit = H.clear

  type 'b iter_initialized = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  let iter_initialized (f : 'b iter_initialized) (m : 'b t) : unit =
    H.iter (fun _ (Pair (_, v)) -> f.iter v) m

  type ('b, 'c) fold_initialized = {
    fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c;
  }

  let fold_initialized (f : ('b, 'c) fold_initialized) (seed : 'c) (m : 'b t) =
    H.fold (fun _ (Pair (_, v)) sofar -> f.fold sofar v) m seed

  type 'b iter_initializedi = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  let iter_initializedi (f : 'b iter_initializedi) (m : 'b t) : unit =
    H.iter (fun _ (Pair (k, v)) -> f.iteri k v) m

  type ('b, 'c) fold_initializedi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  let fold_initializedi (f : ('b, 'c) fold_initializedi) (seed : 'c) (m : 'b t)
      : 'c =
    H.fold (fun _ (Pair (k, v)) sofar -> f.foldi sofar k v) m seed

  type ('b, 'c) map_initializedi = {
    mapi : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> ('a1, 'a2, 'b) data;
  }

  let map_initializedi (f : ('b, 'c) map_initializedi) (m : 'b t) : unit =
    H.filter_map_inplace (fun _ (Pair (k, v)) -> Some (Pair (k, f.mapi k v))) m

  let copy : 'b t -> 'b t = H.copy

  let move ~from ~to_ =
    H.reset to_;
    let aux k v = H.replace to_ k v in
    H.iter aux from
end

module MakeR2 (K : Keys2) : R2 with type ('a, 'b) key = ('a, 'b) K.t = struct
  include
    MakeS2
      (K)
      (struct
        type (_, _, 'b) t = 'b
      end)
end
