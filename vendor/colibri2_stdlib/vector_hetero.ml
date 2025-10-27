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

module type S1 = sig
  type 'a key
  type ('a, 'b) data
  type 'b t

  val create : int -> 'b t
  val size : 'b t -> int
  val get : 'b t -> 'a key -> ('a, 'b) data
  val get_def : 'b t -> 'a key -> ('a, 'b) data -> ('a, 'b) data
  val set : 'b t -> 'a key -> ('a, 'b) data -> unit
  val memo : ('a key -> ('a, 'b) data) -> 'b t -> 'a key -> ('a, 'b) data
  val is_uninitialized : 'b t -> 'a key -> bool
  val uninitialize : 'b t -> 'a key -> unit
  val clear : 'b t -> unit
  val inc_size : 'a key -> 'b t -> unit

  type 'b iter_initialized = { iter : 'a. ('a, 'b) data -> unit }

  val iter_initialized : 'b iter_initialized -> 'b t -> unit

  type ('b, 'c) fold_initialized = { fold : 'a. 'c -> ('a, 'b) data -> 'c }

  val fold_initialized : ('b, 'c) fold_initialized -> 'c -> 'b t -> 'c

  type 'b iter_initializedi = { iteri : 'a. 'a key -> ('a, 'b) data -> unit }

  val iter_initializedi : 'b iter_initializedi -> 'b t -> unit

  type ('b, 'c) fold_initializedi = {
    foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c;
  }

  val fold_initializedi : ('b, 'c) fold_initializedi -> 'c -> 'b t -> 'c
  val copy : 'b t -> 'b t
  (* shallow *)

  val move : from:'b t -> to_:'b t -> unit

  type printk = { printk : 'a. 'a key Fmt.t }
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Fmt.t }

  val pp : unit Fmt.t -> unit Fmt.t -> printk -> 'b printd -> 'b t Fmt.t
end

module Simple_vector = Colibri2_popop_lib.Simple_vector

module MakeS1
    (K : Hashtbl_hetero_sig.Keys1)
    (D : sig
      type ('a, 'b) t
    end) : S1 with type 'a key = 'a K.t and type ('a, 'b) data = ('a, 'b) D.t =
struct
  type 'a key = 'a K.t
  type ('a, 'b) data = ('a, 'b) D.t
  type _ elt = Pair : { k : 'a key; mutable v : ('a, 'b) data } -> 'b elt
  type 'b t = 'b elt Simple_vector.t

  let create = Simple_vector.create
  let size = Simple_vector.size
  let inc_size (type a) (k : a K.t) t = Simple_vector.inc_size (K.tag k + 1) t

  let set (type a) t (k : a K.t) d =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then
        Simple_vector.set t (K.tag k) (Pair { k; v = d })
      else
        match v with
        | Pair r -> (
            match K.equal r.k k with
            | Eq -> r.v <- d
            | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)
    else (
      inc_size k t;
      Simple_vector.set t (K.tag k) (Pair { k; v = d }))

  let open_pair (type a) (k : a K.t) (v : 'b elt) : (a, 'b) data =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get (type a) (t : 'b t) (k : a K.t) : (a, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then raise Not_found
      else open_pair k v
    else raise Not_found

  let get_def (type a) t (k : a K.t) (def : (a, 'b) data) : (a, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then def else open_pair k v
    else def

  let memo f t k =
    match get t k with
    | exception Not_found ->
        let v = f k in
        set t k v;
        v
    | v -> v

  let is_uninitialized (type a) t (k : a K.t) =
    Simple_vector.size t <= K.tag k
    || Simple_vector.is_uninitialized t (K.tag k)

  let uninitialize (type a) t (k : a K.t) =
    Simple_vector.uninitialize t (K.tag k)

  let clear = Simple_vector.clear
  let inc_size (type a) (k : a K.t) t = Simple_vector.inc_size (K.tag k + 1) t

  type 'b iter_initialized = { iter : 'a. ('a, 'b) data -> unit }

  let iter_initialized f t =
    Simple_vector.iter_initialized (function Pair { v } -> f.iter v) t

  type ('b, 'c) fold_initialized = { fold : 'a. 'c -> ('a, 'b) data -> 'c }

  let fold_initialized f acc t =
    Simple_vector.fold_initialized
      (fun acc -> function Pair { v } -> f.fold acc v)
      acc t

  type 'b iter_initializedi = { iteri : 'a. 'a key -> ('a, 'b) data -> unit }

  let iter_initializedi f t =
    Simple_vector.iter_initializedi
      (fun _ -> function Pair { k; v } -> f.iteri k v)
      t

  type ('b, 'c) fold_initializedi = {
    foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c;
  }

  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi
      (fun acc _ -> function Pair { k; v } -> f.foldi acc k v)
      acc t

  let copy t =
    let t = Simple_vector.copy t in
    Simple_vector.apply_initialized
      (function Pair { k; v } -> Pair { k; v })
      t;
    t
  (* shallow *)

  let move = Simple_vector.move
  (* shallow *)

  type printk = { printk : 'a. 'a key Fmt.t }
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Fmt.t }

  let pp sep1 sep2 printkey printdata fmt t =
    let printdata fmt = function
      | Pair { k; v } ->
          printkey.printk fmt k;
          sep2 fmt ();
          printdata.printd k fmt v
    in
    Colibri2_popop_lib.Pp.iter1 Simple_vector.iter_initialized sep1 printdata
      fmt t
end

module type S2 = sig
  type ('a1, 'a2) key
  type ('a1, 'a2, 'b) data
  type 'b t

  val create : int -> 'b t
  val size : 'b t -> int
  val get : 'b t -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data

  val get_def :
    'b t -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> ('a1, 'a2, 'b) data

  val set : 'b t -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit

  val memo :
    (('a1, 'a2) key -> ('a1, 'a2, 'b) data) ->
    'b t ->
    ('a1, 'a2) key ->
    ('a1, 'a2, 'b) data

  val is_uninitialized : 'b t -> ('a1, 'a2) key -> bool
  (** Contrary to Simple_vector it tests the size too *)

  val uninitialize : 'b t -> ('a1, 'a2) key -> unit
  val clear : 'b t -> unit
  val inc_size : ('a1, 'a2) key -> 'b t -> unit

  type 'b iter_initialized = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  val iter_initialized : 'b iter_initialized -> 'b t -> unit

  type ('b, 'c) fold_initialized = {
    fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c;
  }

  val fold_initialized : ('b, 'c) fold_initialized -> 'c -> 'b t -> 'c

  type 'b iter_initializedi = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  val iter_initializedi : 'b iter_initializedi -> 'b t -> unit

  type ('b, 'c) fold_initializedi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  val fold_initializedi : ('b, 'c) fold_initializedi -> 'c -> 'b t -> 'c
  val copy : 'b t -> 'b t
  (* shallow *)

  val move : from:'b t -> to_:'b t -> unit
end

module MakeS2
    (K : Hashtbl_hetero_sig.Keys2)
    (D : sig
      type ('a1, 'a2, 'b) t
    end) :
  S2
    with type ('a1, 'a2) key = ('a1, 'a2) K.t
     and type ('a1, 'a2, 'b) data = ('a1, 'a2, 'b) D.t = struct
  type ('a1, 'a2) key = ('a1, 'a2) K.t
  type ('a1, 'a2, 'b) data = ('a1, 'a2, 'b) D.t

  type _ elt =
    | Pair : { k : ('a1, 'a2) key; mutable v : ('a1, 'a2, 'b) data } -> 'b elt

  type 'b t = 'b elt Simple_vector.t

  let create = Simple_vector.create
  let size = Simple_vector.size
  let inc_size k t = Simple_vector.inc_size (K.tag k + 1) t

  let set (type a1 a2) t (k : (a1, a2) K.t) d =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then
        Simple_vector.set t (K.tag k) (Pair { k; v = d })
      else
        match v with
        | Pair r -> (
            match K.equal r.k k with
            | Eq -> r.v <- d
            | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)
    else (
      inc_size k t;
      Simple_vector.set t (K.tag k) (Pair { k; v = d }))

  let open_pair (type a1 a2) (k : (a1, a2) K.t) (v : 'b elt) : (a1, a2, 'b) data
      =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get (type a1 a2) (t : 'b t) (k : (a1, a2) K.t) : (a1, a2, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then raise Not_found
      else open_pair k v
    else raise Not_found

  let get_def (type a1 a2) t (k : (a1, a2) K.t) (def : (a1, a2, 'b) data) :
      (a1, a2, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then def else open_pair k v
    else def

  let memo f t k =
    match get t k with
    | exception Not_found ->
        let v = f k in
        set t k v;
        v
    | v -> v

  let is_uninitialized (type a1 a2) t (k : (a1, a2) K.t) =
    Simple_vector.size t <= K.tag k
    || Simple_vector.is_uninitialized t (K.tag k)

  let uninitialize (type a1 a2) t (k : (a1, a2) K.t) =
    Simple_vector.uninitialize t (K.tag k)

  let clear = Simple_vector.clear

  let inc_size (type a1 a2) (k : (a1, a2) K.t) t =
    Simple_vector.inc_size (K.tag k + 1) t

  type 'b iter_initialized = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  let iter_initialized f t =
    Simple_vector.iter_initialized (function Pair { v } -> f.iter v) t

  type ('b, 'c) fold_initialized = {
    fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c;
  }

  let fold_initialized f acc t =
    Simple_vector.fold_initialized
      (fun acc -> function Pair { v } -> f.fold acc v)
      acc t

  type 'b iter_initializedi = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  let iter_initializedi f t =
    Simple_vector.iter_initializedi
      (fun _ -> function Pair { k; v } -> f.iteri k v)
      t

  type ('b, 'c) fold_initializedi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi
      (fun acc _ -> function Pair { k; v } -> f.foldi acc k v)
      acc t

  let copy t =
    let t = Simple_vector.copy t in
    Simple_vector.apply_initialized
      (function Pair { k; v } -> Pair { k; v })
      t;
    t
  (* shallow *)

  let move = Simple_vector.move
  (* shallow *)
end

module type R1 = sig
  include S1 with type ('a, 'b) data = 'b

  (* Some primitives get their types simplified *)
  val iter_initialized : ('b -> unit) -> 'b t -> unit
  val fold_initialized : ('c -> 'b -> 'c) -> 'c -> 'b t -> 'c

  val pp :
    unit Format.printer ->
    unit Format.printer ->
    printk ->
    'b Format.printer ->
    'b t Format.printer
end

module MakeR1 (K : Hashtbl_hetero_sig.Keys1) : R1 with type 'a key = 'a K.t =
struct
  type 'a key = 'a K.t
  type ('a, 'b) data = 'b
  type _ elt = Pair : { k : 'a key; mutable v : 'b } -> 'b elt
  type 'b t = 'b elt Simple_vector.t

  let create = Simple_vector.create
  let size = Simple_vector.size
  let inc_size (type a) (k : a K.t) t = Simple_vector.inc_size (K.tag k + 1) t

  let set (type a) t (k : a K.t) d =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then
        Simple_vector.set t (K.tag k) (Pair { k; v = d })
      else
        match v with
        | Pair r -> (
            match K.equal r.k k with
            | Eq -> r.v <- d
            | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)
    else (
      inc_size k t;
      Simple_vector.set t (K.tag k) (Pair { k; v = d }))

  let open_pair (type a) (k : a K.t) (v : 'b elt) : (a, 'b) data =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get (type a) (t : 'b t) (k : a K.t) : (a, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then raise Not_found
      else open_pair k v
    else raise Not_found

  let get_def (type a) t (k : a K.t) (def : (a, 'b) data) : (a, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then def else open_pair k v
    else def

  let memo f t k =
    match get t k with
    | exception Not_found ->
        let v = f k in
        set t k v;
        v
    | v -> v

  let is_uninitialized (type a) t (k : a K.t) =
    Simple_vector.size t <= K.tag k
    || Simple_vector.is_uninitialized t (K.tag k)

  let uninitialize (type a) t (k : a K.t) =
    Simple_vector.uninitialize t (K.tag k)

  let clear = Simple_vector.clear
  let inc_size (type a) (k : a K.t) t = Simple_vector.inc_size (K.tag k + 1) t

  type 'b iter_initialized = { iter : 'a. ('a, 'b) data -> unit }
  type ('b, 'c) fold_initialized = { fold : 'a. 'c -> ('a, 'b) data -> 'c }

  let iter_initialized f t =
    Simple_vector.iter_initialized (function Pair { v } -> f v) t

  let fold_initialized f acc t =
    Simple_vector.fold_initialized
      (fun acc -> function Pair { v } -> f acc v)
      acc t

  type 'b iter_initializedi = { iteri : 'a. 'a key -> 'b -> unit }

  let iter_initializedi f t =
    Simple_vector.iter_initializedi
      (fun _ -> function Pair { k; v } -> f.iteri k v)
      t

  type ('b, 'c) fold_initializedi = { foldi : 'a1. 'c -> 'a1 key -> 'b -> 'c }

  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi
      (fun acc _ -> function Pair { k; v } -> f.foldi acc k v)
      acc t

  let copy t =
    let t = Simple_vector.copy t in
    Simple_vector.apply_initialized
      (function Pair { k; v } -> Pair { k; v })
      t;
    t
  (* shallow *)

  let move = Simple_vector.move
  (* shallow *)

  type printk = { printk : 'a. 'a key Fmt.t }
  type 'b printd = { printd : 'a. 'a key -> 'b Fmt.t }

  let pp sep1 sep2 printkey printdata fmt t =
    let printdata fmt = function
      | Pair { k; v } ->
          printkey.printk fmt k;
          sep2 fmt ();
          printdata fmt v
    in
    Colibri2_popop_lib.Pp.iter1 Simple_vector.iter_initialized sep1 printdata
      fmt t
end

(** Same as S1 but for ('a,'b) data = 'a *)
module type T1 = sig
  type t
  type 'a key

  val create : int -> t
  val size : t -> int
  val get : t -> 'a key -> 'a
  val get_def : t -> 'a key -> 'a -> 'a
  val set : t -> 'a key -> 'a -> unit
  val clear : t -> unit
  val is_uninitialized : t -> 'a key -> bool

  type iter_initialized = { iter : 'a. 'a -> unit }

  val iter_initialized : iter_initialized -> t -> unit

  type 'c fold_initialized = { fold : 'a. 'c -> 'a -> 'c }

  val fold_initialized : 'c fold_initialized -> 'c -> t -> 'c

  type iter_initializedi = { iteri : 'a. 'a key -> 'a -> unit }

  val iter_initializedi : iter_initializedi -> t -> unit

  type 'c fold_initializedi = { foldi : 'a. 'c -> 'a key -> 'a -> 'c }

  val fold_initializedi : 'c fold_initializedi -> 'c -> t -> 'c
  val copy : t -> t
  val move : from:t -> to_:t -> unit

  type printk = { printk : 'a. 'a key Containers.Format.printer }
  type printd = { printd : 'a. 'a key -> 'a Containers.Format.printer }

  val pp :
    unit Containers.Format.printer ->
    unit Containers.Format.printer ->
    printk ->
    printd ->
    t Containers.Format.printer
end

module MakeT1 (K : Hashtbl_hetero_sig.Keys1) : T1 with type 'a key = 'a K.t =
struct
  type 'a key = 'a K.t
  type ('a, 'b) data = 'a
  type elt = Pair : { k : 'a key; mutable v : 'a } -> elt
  type t = elt Simple_vector.t

  let create = Simple_vector.create
  let size = Simple_vector.size
  let inc_size (type a) (k : a K.t) t = Simple_vector.inc_size (K.tag k + 1) t

  let set (type a) t (k : a K.t) d =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then
        Simple_vector.set t (K.tag k) (Pair { k; v = d })
      else
        match v with
        | Pair r -> (
            match K.equal r.k k with
            | Eq -> r.v <- d
            | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)
    else (
      inc_size k t;
      Simple_vector.set t (K.tag k) (Pair { k; v = d }))

  let open_pair (type a) (k : a K.t) (v : elt) : (a, 'b) data =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get (type a) (t : t) (k : a K.t) : (a, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then raise Not_found
      else open_pair k v
    else raise Not_found

  let get_def (type a) t (k : a K.t) (def : (a, 'b) data) : (a, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then def else open_pair k v
    else def

  let is_uninitialized (type a) t (k : a K.t) =
    Simple_vector.size t <= K.tag k
    || Simple_vector.is_uninitialized t (K.tag k)

  let clear = Simple_vector.clear

  type iter_initialized = { iter : 'a. 'a -> unit }

  let iter_initialized f t =
    Simple_vector.iter_initialized (function Pair { v } -> f.iter v) t

  type 'c fold_initialized = { fold : 'a. 'c -> 'a -> 'c }

  let fold_initialized f acc t =
    Simple_vector.fold_initialized
      (fun acc -> function Pair { v } -> f.fold acc v)
      acc t

  type iter_initializedi = { iteri : 'a. 'a key -> 'a -> unit }

  let iter_initializedi f t =
    Simple_vector.iter_initializedi
      (fun _ -> function Pair { k; v } -> f.iteri k v)
      t

  type 'c fold_initializedi = { foldi : 'a. 'c -> 'a key -> 'a -> 'c }

  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi
      (fun acc _ -> function Pair { k; v } -> f.foldi acc k v)
      acc t

  let copy t =
    let t = Simple_vector.copy t in
    Simple_vector.apply_initialized
      (function Pair { k; v } -> Pair { k; v })
      t;
    t
  (* shallow *)

  let move = Simple_vector.move
  (* shallow *)

  type printk = { printk : 'a. 'a key Fmt.t }
  type printd = { printd : 'a. 'a key -> 'a Fmt.t }

  let pp sep1 sep2 printkey printdata fmt t =
    let printdata fmt = function
      | Pair { k; v } ->
          printkey.printk fmt k;
          sep2 fmt ();
          printdata.printd k fmt v
    in
    Colibri2_popop_lib.Pp.iter1 Simple_vector.iter_initialized sep1 printdata
      fmt t
end

module type R2 = sig
  (* type ('a,'b) data = 'b *)
  include S2 with type ('a1, 'a2, 'b) data = 'b
end

module MakeR2 (K : Hashtbl_hetero_sig.Keys2) :
  R2 with type ('a1, 'a2) key = ('a1, 'a2) K.t = struct
  type ('a1, 'a2) key = ('a1, 'a2) K.t
  type ('a1, 'a2, 'b) data = 'b
  type _ elt = Pair : { k : ('a1, 'a2) key; mutable v : 'b } -> 'b elt
  type 'b t = 'b elt Simple_vector.t

  let create = Simple_vector.create
  let size = Simple_vector.size
  let inc_size k t = Simple_vector.inc_size (K.tag k + 1) t

  let set (type a1 a2) t (k : (a1, a2) K.t) d =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then
        Simple_vector.set t (K.tag k) (Pair { k; v = d })
      else
        match v with
        | Pair r -> (
            match K.equal r.k k with
            | Eq -> r.v <- d
            | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)
    else (
      inc_size k t;
      Simple_vector.set t (K.tag k) (Pair { k; v = d }))

  let open_pair (type a1 a2) (k : (a1, a2) K.t) (v : 'b elt) : (a1, a2, 'b) data
      =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get (type a1 a2) (t : 'b t) (k : (a1, a2) K.t) : (a1, a2, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then raise Not_found
      else open_pair k v
    else raise Not_found

  let get_def (type a1 a2) t (k : (a1, a2) K.t) (def : (a1, a2, 'b) data) :
      (a1, a2, 'b) data =
    if K.tag k < Simple_vector.size t then
      let v = Simple_vector.get_dumb t (K.tag k) in
      if Base.phys_equal v Simple_vector.dumb then def else open_pair k v
    else def

  let memo f t k =
    match get t k with
    | exception Not_found ->
        let v = f k in
        set t k v;
        v
    | v -> v

  let is_uninitialized (type a1 a2) t (k : (a1, a2) K.t) =
    Simple_vector.size t <= K.tag k
    || Simple_vector.is_uninitialized t (K.tag k)

  let uninitialize (type a1 a2) t (k : (a1, a2) K.t) =
    Simple_vector.uninitialize t (K.tag k)

  let clear = Simple_vector.clear

  let inc_size (type a1 a2) (k : (a1, a2) K.t) t =
    Simple_vector.inc_size (K.tag k + 1) t

  type 'b iter_initialized = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  let iter_initialized f t =
    Simple_vector.iter_initialized (function Pair { v } -> f.iter v) t

  type ('b, 'c) fold_initialized = {
    fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c;
  }

  let fold_initialized f acc t =
    Simple_vector.fold_initialized
      (fun acc -> function Pair { v } -> f.fold acc v)
      acc t

  type 'b iter_initializedi = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  let iter_initializedi f t =
    Simple_vector.iter_initializedi
      (fun _ -> function Pair { k; v } -> f.iteri k v)
      t

  type ('b, 'c) fold_initializedi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi
      (fun acc _ -> function Pair { k; v } -> f.foldi acc k v)
      acc t

  let copy t =
    let t = Simple_vector.copy t in
    Simple_vector.apply_initialized
      (function Pair { k; v } -> Pair { k; v })
      t;
    t
  (* shallow *)

  let move = Simple_vector.move
  (* shallow *)
end
