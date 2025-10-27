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

(** imperative, extensible and heterogene Arrays *)
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

  type ('b, 'c) foldi = {
    foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c;
  }[@@unboxed]

  val foldi : ('b, 'c) foldi -> 'c -> 'b t -> 'c
  val copy : 'b t -> 'b t
  (* shallow *)

  val move : from:'b t -> to_:'b t -> unit

  type printk = { printk : 'a. 'a key Fmt.t }[@@unboxed]
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Fmt.t }[@@unboxed]

  val pp : unit Fmt.t -> unit Fmt.t -> printk -> 'b printd -> 'b t Fmt.t
end

module MakeS1
  (K:Hashtbl_hetero_sig.Keys1)
  (D:sig type ('a,'b) t end)
  : S1 with type 'a key = 'a K.t and type ('a,'b) data = ('a,'b) D.t

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
  (K:Hashtbl_hetero_sig.Keys2)
  (D:sig type ('a1,'a2,'b) t end)
  : S2 with type ('a1,'a2) key = ('a1,'a2) K.t
        and type ('a1,'a2,'b) data = ('a1,'a2,'b) D.t


(** The following are needed in order to avoid ('a,'b) t = 'b in an
    instanciation of the previous functors
    (cf. ocaml mantis #5083:
    J.Garrigue : "Phantom types must be either abstract or private.
    In particular, using an abbreviation for a phantom type is just
    a Russian roulette.")
*)

(** Same as S1 but for ('a,'b) data = 'b *)
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

module MakeR1
  (K:Hashtbl_hetero_sig.Keys1)
  : R1 with type 'a key = 'a K.t

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

module MakeT1
  (K:Hashtbl_hetero_sig.Keys1)
  : T1 with type 'a key = 'a K.t

  module type R2 = sig
    (* type ('a,'b) data = 'b *)
    include S2 with type ('a1, 'a2, 'b) data = 'b
  end
  
  module MakeR2
  (K:Hashtbl_hetero_sig.Keys2)
  : R2 with type ('a1,'a2) key = ('a1,'a2) K.t
