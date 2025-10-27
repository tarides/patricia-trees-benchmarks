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
  type 'b init = { init : 'a. 'a key -> ('a, 'b) data } [@@unboxed]

  val create : 'b init -> 'b t
  (** Initialize with current keys, it can't be used with futur keys *)

  val get : 'b t -> 'a key -> ('a, 'b) data
  val set : 'b t -> 'a key -> ('a, 'b) data -> unit

  type 'b iter = { iter : 'a. ('a, 'b) data -> unit } [@@unboxed]

  val iter : 'b iter -> 'b t -> unit

  type ('b, 'c) fold = { fold : 'a. 'c -> ('a, 'b) data -> 'c } [@@unboxed]

  val fold : ('b, 'c) fold -> 'c -> 'b t -> 'c

  type 'b iteri = { iteri : 'a. 'a key -> ('a, 'b) data -> unit } [@@unboxed]

  val iteri : 'b iteri -> 'b t -> unit

  type ('b, 'c) foldi = { foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c }
  [@@unboxed]

  val foldi : ('b, 'c) foldi -> 'c -> 'b t -> 'c
  val copy : 'b t -> 'b t
  (* shallow *)

  val move : from:'b t -> to_:'b t -> unit

  type printk = { printk : 'a. 'a key Fmt.t } [@@unboxed]
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Fmt.t } [@@unboxed]

  val pp : unit Fmt.t -> unit Fmt.t -> printk -> 'b printd -> 'b t Fmt.t
end

module MakeS1
    (K : Hashtbl_hetero_sig.Keys1)
    (D : sig
      type ('a, 'b) t
    end) : S1 with type 'a key = 'a K.t and type ('a, 'b) data = ('a, 'b) D.t =
struct
  type 'a key = 'a K.t
  type ('a, 'b) data = ('a, 'b) D.t
  type _ elt = Pair : { k : 'a key; mutable v : ('a, 'b) data } -> 'b elt
  type 'b t = 'b elt Array.t
  type 'b init = { init : 'a. 'a key -> ('a, 'b) data } [@@unboxed]

  let create (type b) init : b t =
    let a =
      Array.make (K.size ()) (Sys.opaque_identity (Obj.magic () : b elt))
    in
    let iter (type a) (k : a K.t) : unit =
      Array.set a (K.tag k) (Pair { k; v = init.init k })
    in
    K.iter { iter };
    a

  let set (type a) t (k : a K.t) (d : (a, 'b) data) =
    let v = Array.get t (K.tag k) in
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v <- d
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let open_pair (type a) (k : a K.t) (v : 'b elt) : (a, 'b) data =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get t k = open_pair k (Array.get t (K.tag k))

  type 'b iter = { iter : 'a. ('a, 'b) data -> unit } [@@unboxed]

  let iter f t = Array.iter (function Pair { v } -> f.iter v) t

  type ('b, 'c) fold = { fold : 'a. 'c -> ('a, 'b) data -> 'c } [@@unboxed]

  let fold f acc t =
    Array.fold (fun acc -> function Pair { v } -> f.fold acc v) acc t

  type 'b iteri = { iteri : 'a. 'a key -> ('a, 'b) data -> unit } [@@unboxed]

  let iteri f t = Array.iter (function Pair { k; v } -> f.iteri k v) t

  type ('b, 'c) foldi = { foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c }
  [@@unboxed]

  let foldi f acc t =
    Array.fold (fun acc -> function Pair { k; v } -> f.foldi acc k v) acc t

  let copy t = Array.map (function Pair { k; v } -> Pair { k; v }) t
  (* shallow *)

  let move ~from ~to_ =
    assert (Array.length to_ = Array.length from);
    Array.blit from 0 to_ 0 (Array.length from)
  (* shallow *)

  type printk = { printk : 'a. 'a key Fmt.t } [@@unboxed]
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Fmt.t } [@@unboxed]

  let pp sep1 sep2 printkey printdata fmt t =
    let printdata fmt = function
      | Pair { k; v } ->
          printkey.printk fmt k;
          sep2 fmt ();
          printdata.printd k fmt v
    in
    Colibri2_popop_lib.Pp.iter1 Array.iter sep1 printdata fmt t
end

module type S2 = sig
  type ('a1, 'a2) key
  type ('a1, 'a2, 'b) data
  type 'b t

  type 'b init = { init : 'a1 'a2. ('a1,'a2) key -> ('a1, 'a2, 'b) data } [@@unboxed]

  val create : 'b init -> 'b t
  (** Initialize with current keys, it can't be used with futur keys *)
  val get : 'b t -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data
  val set : 'b t -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit

  type 'b iter = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  val iter : 'b iter -> 'b t -> unit

  type ('b, 'c) fold = { fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c }

  val fold : ('b, 'c) fold -> 'c -> 'b t -> 'c

  type 'b iteri = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  val iteri : 'b iteri -> 'b t -> unit

  type ('b, 'c) foldi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  val foldi : ('b, 'c) foldi -> 'c -> 'b t -> 'c
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

  type 'b t = 'b elt Array.t

  type 'b init = { init : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data }
  [@@unboxed]

  let create (type b) init : b t =
    let a =
      Array.make (K.size ()) (Sys.opaque_identity (Obj.magic () : b elt))
    in
    let iter (type a1 a2) (k : (a1, a2) K.t) : unit =
      Array.set a (K.tag k) (Pair { k; v = init.init k })
    in
    K.iter { iter };
    a

  let set (type a1 a2) t (k : (a1, a2) K.t) (d : (a1, a2, 'b) data) =
    let v = Array.get t (K.tag k) in
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v <- d
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let open_pair (type a1 a2) (k : (a1, a2) K.t) (v : 'b elt) : (a1, a2, 'b) data
      =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get t k = open_pair k (Array.get t (K.tag k))

  type 'b iter = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  let iter f t = Array.iter (function Pair { v } -> f.iter v) t

  type ('b, 'c) fold = { fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c }

  let fold f acc t =
    Array.fold (fun acc -> function Pair { v } -> f.fold acc v) acc t

  type 'b iteri = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  let iteri f t =
    Array.iteri (fun _ -> function Pair { k; v } -> f.iteri k v) t

  type ('b, 'c) foldi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  let foldi f acc t =
    Array.foldi (fun acc _ -> function Pair { k; v } -> f.foldi acc k v) acc t

  let copy t = Array.map (function Pair { k; v } -> Pair { k; v }) t
  (* shallow *)

  let move ~from ~to_ =
    assert (Array.length to_ = Array.length from);
    Array.blit from 0 to_ 0 (Array.length from)
end

module type R1 = sig
  include S1 with type ('a, 'b) data = 'b

  (* Some primitives get their types simplified *)
  val iter : ('b -> unit) -> 'b t -> unit
  val fold : ('c -> 'b -> 'c) -> 'c -> 'b t -> 'c

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
  type _ elt = Pair : { k : 'a key; mutable v : ('a, 'b) data } -> 'b elt
  type 'b t = 'b elt Array.t
  type 'b init = { init : 'a. 'a key -> ('a, 'b) data } [@@unboxed]

  let create (type b) init : b t =
    let a =
      Array.make (K.size ()) (Sys.opaque_identity (Obj.magic () : b elt))
    in
    let iter (type a) (k : a K.t) : unit =
      Array.set a (K.tag k) (Pair { k; v = init.init k })
    in
    K.iter { iter };
    a

  let set (type a) t (k : a K.t) (d : (a, 'b) data) =
    let v = Array.get t (K.tag k) in
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v <- d
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let open_pair (type a) (k : a K.t) (v : 'b elt) : (a, 'b) data =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get (type a) (t : 'b t) (k : a K.t) : (a, 'b) data =
    open_pair k (Array.get t (K.tag k))

  type 'b iter = { iter : 'a. ('a, 'b) data -> unit } [@@unboxed]

  let iter f t = Array.iter (function Pair { v } -> f v) t

  type ('b, 'c) fold = { fold : 'a. 'c -> ('a, 'b) data -> 'c } [@@unboxed]

  let fold f acc t =
    Array.fold (fun acc -> function Pair { v } -> f acc v) acc t

  type 'b iteri = { iteri : 'a. 'a key -> ('a, 'b) data -> unit } [@@unboxed]

  let iteri f t = Array.iter (function Pair { k; v } -> f.iteri k v) t

  type ('b, 'c) foldi = { foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c }
  [@@unboxed]

  let foldi f acc t =
    Array.fold (fun acc -> function Pair { k; v } -> f.foldi acc k v) acc t

  let copy t = Array.map (function Pair { k; v } -> Pair { k; v }) t
  (* shallow *)

  let move ~from ~to_ =
    assert (Array.length to_ = Array.length from);
    Array.blit from 0 to_ 0 (Array.length from)
  (* shallow *)

  type printk = { printk : 'a. 'a key Fmt.t } [@@unboxed]
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Fmt.t } [@@unboxed]

  let pp sep1 sep2 printkey printdata fmt t =
    let printdata fmt = function
      | Pair { k; v } ->
          printkey.printk fmt k;
          sep2 fmt ();
          printdata fmt v
    in
    Colibri2_popop_lib.Pp.iter1 Array.iter sep1 printdata fmt t
end

(** Same as S1 but for ('a,'b) data = 'a *)
module type T1 = sig
  type t
  type 'a key

  type init = { init : 'a. 'a key -> 'a } [@@unboxed]

  val create : init -> t
  val get : t -> 'a key -> 'a
  val set : t -> 'a key -> 'a -> unit

  type iter = { iter : 'a. 'a -> unit }[@@unboxed]

  val iter : iter -> t -> unit

  type 'c fold = { fold : 'a. 'c -> 'a -> 'c }[@@unboxed]

  val fold : 'c fold -> 'c -> t -> 'c

  type iteri = { iteri : 'a. 'a key -> 'a -> unit }[@@unboxed]

  val iteri : iteri -> t -> unit

  type 'c foldi = { foldi : 'a. 'c -> 'a key -> 'a -> 'c }[@@unboxed]

  val foldi : 'c foldi -> 'c -> t -> 'c
  val copy : t -> t
  val move : from:t -> to_:t -> unit

  type printk = { printk : 'a. 'a key Containers.Format.printer }[@@unboxed]
  type printd = { printd : 'a. 'a key -> 'a Fmt.t } [@@unboxed]

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
  type t = elt Array.t
  type init = { init : 'a. 'a key -> 'a } [@@unboxed]

  let create init : t =
    let a =
      Array.make (K.size ()) (Sys.opaque_identity (Obj.magic () : elt))
    in
    let iter (type a) (k : a K.t) : unit =
      Array.set a (K.tag k) (Pair { k; v = init.init k })
    in
    K.iter { iter };
    a

  let set (type a) t (k : a K.t) (d : (a, 'b) data) =
    let v = Array.get t (K.tag k) in
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v <- d
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let open_pair (type a) (k : a K.t) (v : 'elt) : (a, 'b) data =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get t k = open_pair k (Array.get t (K.tag k))

  type iter = { iter : 'a. 'a -> unit } [@@unboxed]

  let iter f t = Array.iter (function Pair { v } -> f.iter v) t

  type 'c fold = { fold : 'a. 'c -> 'a -> 'c } [@@unboxed]

  let fold f acc t =
    Array.fold (fun acc -> function Pair { v } -> f.fold acc v) acc t

  type iteri = { iteri : 'a. 'a key -> 'a-> unit } [@@unboxed]

  let iteri f t = Array.iter (function Pair { k; v } -> f.iteri k v) t

  type 'c foldi = { foldi : 'a. 'c -> 'a key -> 'a-> 'c }
  [@@unboxed]

  let foldi f acc t =
    Array.fold (fun acc -> function Pair { k; v } -> f.foldi acc k v) acc t

  let copy t = Array.map (function Pair { k; v } -> Pair { k; v }) t
  (* shallow *)

  let move ~from ~to_ =
    assert (Array.length to_ = Array.length from);
    Array.blit from 0 to_ 0 (Array.length from)
  (* shallow *)

  type printk = { printk : 'a. 'a key Fmt.t } [@@unboxed]
  type printd = { printd : 'a. 'a key -> 'a Fmt.t } [@@unboxed]

  let pp sep1 sep2 printkey printdata fmt t =
    let printdata fmt = function
      | Pair { k; v } ->
          printkey.printk fmt k;
          sep2 fmt ();
          printdata.printd k fmt v
    in
    Colibri2_popop_lib.Pp.iter1 Array.iter sep1 printdata fmt t
end

module type R2 = sig
  (* type ('a,'b) data = 'b *)
  include S2 with type ('a1, 'a2, 'b) data = 'b
end

module MakeR2
    (K : Hashtbl_hetero_sig.Keys2)
:
  R2
    with type ('a1, 'a2) key = ('a1, 'a2) K.t
    = struct
  type ('a1, 'a2) key = ('a1, 'a2) K.t
  type ('a1, 'a2, 'b) data = 'b

  type _ elt =
    | Pair : { k : ('a1, 'a2) key; mutable v : ('a1, 'a2, 'b) data } -> 'b elt

  type 'b t = 'b elt Array.t

  type 'b init = { init : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data }
  [@@unboxed]

  let create (type b) init : b t =
    let a =
      Array.make (K.size ()) (Sys.opaque_identity (Obj.magic () : b elt))
    in
    let iter (type a1 a2) (k : (a1, a2) K.t) : unit =
      Array.set a (K.tag k) (Pair { k; v = init.init k })
    in
    K.iter { iter };
    a

  let set (type a1 a2) t (k : (a1, a2) K.t) (d : (a1, a2, 'b) data) =
    let v = Array.get t (K.tag k) in
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v <- d
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let open_pair (type a1 a2) (k : (a1, a2) K.t) (v : 'b elt) : (a1, a2, 'b) data
      =
    match v with
    | Pair r -> (
        match K.equal r.k k with
        | Eq -> r.v
        | Neq -> raise Hashtbl_hetero_sig.IncoherentTable)

  let get t k = open_pair k (Array.get t (K.tag k))

  type 'b iter = { iter : 'a1 'a2. ('a1, 'a2, 'b) data -> unit }

  let iter f t = Array.iter (function Pair { v } -> f.iter v) t

  type ('b, 'c) fold = { fold : 'a1 'a2. 'c -> ('a1, 'a2, 'b) data -> 'c }

  let fold f acc t =
    Array.fold (fun acc -> function Pair { v } -> f.fold acc v) acc t

  type 'b iteri = {
    iteri : 'a1 'a2. ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> unit;
  }

  let iteri f t =
    Array.iteri (fun _ -> function Pair { k; v } -> f.iteri k v) t

  type ('b, 'c) foldi = {
    foldi : 'a1 'a2. 'c -> ('a1, 'a2) key -> ('a1, 'a2, 'b) data -> 'c;
  }

  let foldi f acc t =
    Array.foldi (fun acc _ -> function Pair { k; v } -> f.foldi acc k v) acc t

  let copy t = Array.map (function Pair { k; v } -> Pair { k; v }) t
  (* shallow *)

  let move ~from ~to_ =
    assert (Array.length to_ = Array.length from);
    Array.blit from 0 to_ 0 (Array.length from)
end
