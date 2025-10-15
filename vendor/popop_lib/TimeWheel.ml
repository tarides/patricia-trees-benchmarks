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

open Base

(** TimeWheel *)

module type S = sig
  type 'a t
  type context

  val create : context -> 'a t

  val add : 'a t -> 'a -> int -> unit
  (** [add t v offset] add the event v at the given offset in the futur *)

  val next : 'a t -> 'a option
  val next_at_same_time : 'a t -> 'a option
  val find_next : 'a t -> unit
  val current_time : 'a t -> int
  val size : 'a t -> int
  val size_at_current_time : 'a t -> int
end

module Make (Context : sig
  type t
end) (Array : sig
  type 'a t

  val create : Context.t -> int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end) (Ref : sig
  type 'a t

  val create : Context.t -> 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end) : S with type context := Context.t = struct
  type 'a cell = Nil | Cons of { v : 'a; time : int; next : 'a cell }
  type 'a t = { current : int Ref.t; futur : 'a cell Array.t; size : int Ref.t }

  let create ctx =
    {
      current = Ref.create ctx 0;
      futur = Array.create ctx Int.num_bits Nil;
      size = Ref.create ctx 0;
    }

  let current_time t = Ref.get t.current
  let size t = Ref.get t.size

  let bucket t time =
    assert (Ref.get t.current <= time);
    let i = Int.num_bits - Int.clz (time - Ref.get t.current) in
    (* Caml.Format.eprintf "-> t.current = %i; time = %i; i = %i@." (Ref.get t.current) time i; *)
    i

  let add_in_bucket t v time =
    let b = bucket t time in
    Array.set t.futur b (Cons { v; time; next = Array.get t.futur b })

  let add t v offset =
    assert (1 <= offset);
    let time = Ref.get t.current + offset in
    Ref.set t.size (Ref.get t.size + 1);
    add_in_bucket t v time

  (* optim *)

  (* let rec find_smallest i =
   *   if Int.num_bits <= i then None
   *   else
   *     match t.futur.(i) with
   *     | Nil -> find_smallest (i+1)
   *     | Cons _ as b ->
   *       let rec find_smallest acc = function
   *         | Nil -> Some acc
   *         | Cons { time; next } ->
   *           find_smallest (min time acc) next
   *       in
   *       find_smallest Int.max_value b
   * in
   * match find_smallest 0 with
   * | None -> ()
   * | Some next ->
   *   assert ( t.current < next );
   *   (\* We compute all the bit that would have been the at 1 from 0, if we would
   *      go from t.current to next by 1 increment.Arith_status
   *      All the bits are in different categories :
   *       - The leading unchanged bits
   *       - The biggest set bit
   *       - The bits that went from 1 to 0, before the biggest bit is set
   *       - The bits that stayed at 0, after the biggest bit is set
   *       - The other below those that changed multiple times.
   *   *\)
   *   let biggest = Int.clz (next land (lnot t.current)) in
   *   let mask = -1 lsl biggest in
   *   let before_1_0 = Int.clz (lnot (t.current lor mask)) in
   *   let before_stay_0 = Int.clz (t.next land (lnot mask)) in *)

  let incr_current t =
    let bitset = Int.ctz (lnot (Ref.get t.current)) in
    let bitset = bitset + 2 in
    Ref.set t.current (Ref.get t.current + 1);
    let empty i =
      (* Caml.Format.eprintf "empty t.current = %i; i = %i@." (Ref.get t.current) i; *)
      let b = Array.get t.futur i in
      Array.set t.futur i Nil;
      let rec aux = function
        | Nil -> ()
        | Cons { v; time; next } ->
            (* Caml.Format.eprintf "t.current = %i; time = %i; i = %i@." (Ref.get t.current) time i; *)
            assert (Ref.get t.current <= time);
            add_in_bucket t v time;
            aux next
      in
      aux b
    in
    empty 1;
    (* always emptied because next but could be just copied *)
    empty bitset

  let find_next t =
    if 0 < Ref.get t.size then
      let rec aux t =
        match Array.get t.futur 0 with
        | Nil ->
            incr_current t;
            aux t
        | Cons _ -> ()
      in
      aux t

  let next_at_same_time t =
    match Array.get t.futur 0 with
    | Nil -> None
    | Cons { v; time; next } ->
        assert (Int.equal time (Ref.get t.current));
        Ref.set t.size (Ref.get t.size - 1);
        Array.set t.futur 0 next;
        Some v

  let next t =
    find_next t;
    next_at_same_time t

  let size_at_current_time t =
    let rec aux acc = function
      | Nil -> acc
      | Cons { next; _ } -> aux (acc + 1) next
    in
    aux 0 (Array.get t.futur 0)
end

include
  Make
    (struct
      type t = unit
    end)
    (struct
      type 'a t = 'a array

      let create () i v = Array.create ~len:i v
      let get = Array.get
      let set = Array.set
    end)
    (struct
      type 'a t = 'a ref

      let create () v = ref v
      let get = Ref.( ! )
      let set = Ref.( := )
    end)
