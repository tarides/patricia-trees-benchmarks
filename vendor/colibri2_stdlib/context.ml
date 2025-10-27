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

type context = {
  mutable bps : bp list;
  when_pushing : (unit -> unit) Vector.vector;
  before_pushing : (unit -> unit) Vector.vector;
}

and bp = {
  mutable alive : bool;  (** not poped *)
  context : context;
  when_poping : (unit -> unit) Vector.vector;
}

type creator = context

let creator t = t
let bp_equal (a : bp) b = CCEqual.physical a b

let create () =
  let rec context =
    {
      bps = [ bp ];
      when_pushing = Vector.create ();
      before_pushing = Vector.create ();
    }
  and bp = { alive = true; context; when_poping = Vector.create () } in
  context

let first_bp t = Base.List.last_exn t.bps

let bp t =
  match t.bps with
  | [] ->
      assert false
      (* absurd: the level0 can't be removed since there is no bp under it *)
  | bp :: _ -> bp

let push context =
  Vector.iter (fun f -> f ()) context.before_pushing;
  let bp = { alive = true; context; when_poping = Vector.create () } in
  context.bps <- bp :: context.bps;
  Vector.iter (fun f -> f ()) context.when_pushing

exception AlreadyPoped

let pop bp =
  if not bp.alive then raise AlreadyPoped;
  let rec aux = function
    | [] -> assert false (* absurd: by invariant bp must be in the list *)
    | a :: _ as l when bp_equal a bp -> bp.context.bps <- l
    | a :: l ->
        assert a.alive;
        a.alive <- false;
        Vector.iter (fun f -> f ()) a.when_poping;
        aux l
  in
  aux bp.context.bps

let do_when_poping_current_bp context f =
  let top_bp = bp context in
  Vector.push top_bp.when_poping f

let always_do_when_pushing context f = Vector.push context.when_pushing f
let always_do_before_pushing context f = Vector.push context.before_pushing f

module Ref = struct
  type 'a t = { mutable contents : 'a; mutable bp : bp }

  let create context x = { contents = x; bp = first_bp context }

  let set_aux (r : _ t) v =
    let top_bp = bp r.bp.context in
    if bp_equal r.bp top_bp then r.contents <- v
    else
      let old = r.contents in
      let old_bp = r.bp in
      Vector.push top_bp.when_poping (fun () ->
          r.contents <- old;
          r.bp <- old_bp);
      r.contents <- v;
      r.bp <- top_bp

  let set (r : 'a t) v = if not (CCEqual.physical r.contents v) then set_aux r v
  let get (r : 'a t) = r.contents

  let incr ?trace r =
    set_aux r (1 + r.contents);
    Option.iter (fun trace -> Trace_core.counter_int trace r.contents) trace

  let creator (h : 'a t) = h.bp.context
  let pp pp fmt r = pp fmt (get r)
end

module Expert = struct
  type 'a history = { mutable previous : 'a hist list; context : context }
  and 'a hist = { saved : 'a; at : bp }

  let refresh ~restore t h =
    match h.previous with
    | [] -> ()
    | { at } :: _ when at.alive -> ()
    | { saved } :: l ->
        let rec aux saved = function
          | { at; saved } :: l when not at.alive -> aux saved l
          | l ->
              restore t saved;
              h.previous <- l
        in
        aux saved l

  let save ~restore ~save t h =
    refresh ~restore t h;
    match h.previous with
    | { at } :: _ when bp_equal at (bp h.context) -> ()
    | _ -> h.previous <- { at = bp h.context; saved = save t } :: h.previous

  let create context = { previous = []; context }
end

type 'a history = { mutable bp : bp }

module Make (S : sig
  type t
  type saved

  val save : t -> saved
  val restore : saved -> t -> unit
  val get_history : t -> saved history
end) =
struct
  let create context = { bp = first_bp context }
  let refresh _ = ()

  let save t =
    let h = S.get_history t in
    let old_bp = h.bp in
    let top_bp = bp old_bp.context in
    if bp_equal old_bp top_bp then ()
    else
      let saved = S.save t in
      Vector.push top_bp.when_poping (fun () ->
          S.restore saved t;
          h.bp <- old_bp);
      h.bp <- top_bp

  type hidden = S.t

  let ro t =
    refresh t;
    t

  let rw t =
    save t;
    t

  let hide t = t
  let creator (h : 'a history) = h.bp.context
end

module Basic (S : sig
  type t
  type saved

  val save : t -> saved
  val restore : saved -> t -> unit
end) =
struct
  type t = { d : S.t; history : S.saved history }

  module M = Make (struct
    type nonrec t = t
    type saved = S.saved

    let save t = S.save t.d
    let restore saved t = S.restore saved t.d
    let get_history t = t.history
  end)

  let create context d = { d; history = M.create context }

  let get d =
    M.save d;
    d.d
end

module Push = struct
  type 'a t = { v : 'a Vector.vector; h : int Expert.history }

  let restore t size = Vector.truncate t.v size
  let save t = Vector.size t.v
  let refresh t = Expert.refresh ~restore t t.h
  let save t = Expert.save ~restore ~save t t.h
  let create creator = { v = Vector.create (); h = Expert.create creator }

  let push t v =
    save t;
    Vector.push t.v v

  let iter f t =
    refresh t;
    Vector.iter f t.v

  let fold f acc t =
    refresh t;
    Vector.fold f acc t.v

  let exists f t =
    refresh t;
    Vector.exists f t.v

  let length t =
    refresh t;
    Vector.length t.v

  let get t i =
    refresh t;
    Vector.get t.v i

  let to_seq t =
    refresh t;
    Std.Sequence.unfold ~init:(Vector.length t.v) ~f:(fun i ->
        if i = 0 then None
        else
          let i = i - 1 in
          Some (Vector.get t.v i, i))

  let pp ?sep pp fmt v = Vector.pp ?pp_sep:sep pp fmt v.v
end

module Queue = struct
  type 'a t = { v : 'a Base.Queue.t; h : 'a Base.Queue.t option Expert.history }

  let restore t saved =
    Base.Queue.clear t.v;
    match saved with
    | None -> ()
    | Some src -> Base.Queue.blit_transfer ~src ~dst:t.v ()

  let save t =
    if Base.Queue.is_empty t.v then None else Some (Base.Queue.copy t.v)

  let refresh t = Expert.refresh ~restore t t.h
  let save t = Expert.save ~restore ~save t t.h
  let create creator = { v = Base.Queue.create (); h = Expert.create creator }

  let enqueue t v =
    save t;
    Base.Queue.enqueue t.v v

  let dequeue t =
    save t;
    Base.Queue.dequeue t.v

  let iter f t =
    refresh t;
    Base.Queue.iter ~f t.v

  let fold f acc t =
    refresh t;
    Base.Queue.fold ~f ~init:acc t.v

  let length t =
    refresh t;
    Base.Queue.length t.v

  let get t i =
    refresh t;
    Base.Queue.get t.v i

  let is_empty t =
    refresh t;
    Base.Queue.is_empty t.v

  let of_seq t =
    refresh t;
    Std.Sequence.unfold ~init:(Base.Queue.length t.v) ~f:(fun i ->
        if i = 0 then None
        else
          let i = i - 1 in
          Some (Base.Queue.get t.v i, i))

  let pp ?(sep = Fmt.nop) pp fmt v =
    Base.Queue.iteri v.v ~f:(fun i v ->
        if i <> 0 then sep fmt ();
        pp fmt v)
end

module RefOpt = struct
  type 'a t = 'a option Ref.t

  let create creator = Ref.create creator None
  let set t v = Ref.set t (Some v)
  let unset t = Ref.set t None
  let get t = Ref.get t
  (* let get_exn exn t = match Ref.get t with None -> raise exn | Some x -> x
     let get_def ~def t = match Ref.get t with None -> def | Some x -> x *)
end

module type Hashtbl = sig
  type 'a t
  type key

  val pp : 'a Fmt.t -> 'a t Fmt.t
  val create : creator -> 'a t
  val remove : 'a t -> key -> unit
  val set : 'a t -> key -> 'a -> unit
  val set_opt : 'a t -> key -> 'a option -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_def : 'a t -> def:'a -> key -> 'a
  val find_exn : 'a t -> exn -> key -> 'a
  val find_remove : 'a t -> key -> 'a option
  val mem : 'a t -> key -> bool
  val change : ('a option -> 'a option) -> 'a t -> key -> unit
  val add_change : ('b -> 'a) -> ('b -> 'a -> 'a) -> 'a t -> key -> 'b -> unit
  val choose : 'a t -> (key * 'a) option
  val iter : f:(key -> 'a -> unit) -> 'a t -> unit
  val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val clear : 'a t -> unit
  val length : 'a t -> int
end

module Hashtbl (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  Hashtbl with type key := S.t = struct
  type before = B
  type last = L

  let _ = B
  let _ = L

  type (_, _) data =
    | Set : {
        bp : bp;
        next : ('a, 'c) data;
        mutable v : 'a;
      }
        -> ('a, before) data
    | Unset : { bp : bp; next : ('a, 'c) data } -> ('a, before) data
    | End : ('a, last) data

  type 'a t = {
    (* The table has multiple values for each keys *)
    h : ('a, before) data S.H.t;
    to_remove : S.t Vector.vector;
    mutable bp : bp;
    context : context;
  }

  let restore t size =
    for i = size to Vector.length t.to_remove - 1 do
      let k = Vector.get t.to_remove i in
      let aux : type a. (_, a) data -> (_, before) data option =
       fun next ->
        match next with
        | End -> None
        | Set _ -> Some next
        | Unset _ -> Some next
      in
      S.H.change
        (function
          | None -> None
          | Some (Set { next }) -> aux next
          | Some (Unset { next }) -> aux next)
        t.h k
    done;
    Vector.truncate t.to_remove size

  let save (t : _ t) =
    let old_bp = t.bp in
    let top_bp = bp t.context in
    if bp_equal t.bp top_bp then ()
    else
      let saved = Vector.size t.to_remove in
      Vector.push top_bp.when_poping (fun () ->
          restore t saved;
          t.bp <- old_bp);
      t.bp <- top_bp

  (** todo print only last binding *)
  let pp pp fmt t =
    let iter f =
      S.H.iter (fun k r -> match r with Set { v } -> f k v | _ -> ())
    in
    Colibri2_popop_lib.Pp.(iter2 iter semi arrow S.pp pp) fmt t.h

  let create context =
    {
      h = S.H.create 10;
      context;
      bp = first_bp context;
      to_remove = Vector.create ();
    }

  let find t k =
    let r = S.H.find t.h k in
    match r with Set r -> r.v | Unset _ -> raise Not_found

  let find_opt t k =
    let open Option in
    let* r = S.H.find_opt t.h k in
    match r with Set r -> Some r.v | Unset _ -> None

  let find_exn t exn k =
    match S.H.find_exn t.h exn k with Set { v } -> v | Unset _ -> raise exn

  let find_def t ~def k =
    match S.H.find_opt t.h k with
    | None | Some (Unset _) -> def
    | Some (Set r) -> r.v

  let add_binding t k v next =
    S.H.replace t.h k (Set { v; bp = t.bp; next });
    Vector.push t.to_remove k

  let add_unset_binding t k next =
    S.H.replace t.h k (Unset { bp = t.bp; next });
    Vector.push t.to_remove k

  let find_remove t k =
    save t;
    let open Option in
    let* l = S.H.find_opt t.h k in
    match l with
    | Set r ->
        if bp_equal r.bp t.bp then
          S.H.replace t.h k (Unset { bp = t.bp; next = r.next })
        else add_unset_binding t k l;
        Some r.v
    | Unset _ -> None

  let mem t k = Option.is_some (find_opt t k)

  let set t k v =
    save t;
    match S.H.find_opt t.h k with
    | Some (Set r as l) ->
        if bp_equal r.bp t.bp then r.v <- v else add_binding t k v l
    | Some (Unset r as l) ->
        if bp_equal r.bp t.bp then
          S.H.replace t.h k (Set { v; bp = t.bp; next = r.next })
        else add_binding t k v l
    | None -> add_binding t k v End

  let remove t k =
    save t;
    match S.H.find_opt t.h k with
    | None | Some (Unset _) -> ()
    | Some (Set r as l) ->
        if bp_equal r.bp t.bp then
          S.H.replace t.h k (Unset { bp = t.bp; next = r.next })
        else add_unset_binding t k l

  let set_opt t k v = match v with None -> remove t k | Some x -> set t k x

  let change f (t : 'a t) k =
    let v = f (find_opt t k) in
    set_opt t k v

  let add_change first add (t : 'a t) k v =
    match find_opt t k with
    | None -> set t k (first v)
    | Some v' -> set t k (add v v')

  let choose (type a) (t : a t) =
    let exception Found of (S.t * a) in
    try
      S.H.iter
        (fun k v ->
          match v with Set { v } -> raise_notrace (Found (k, v)) | _ -> ())
        t.h;
      None
    with Found (key, v) -> Some (key, v)

  let iter ~f t =
    S.H.iter (fun k v -> match v with Set { v } -> f k v | _ -> ()) t.h

  let fold f acc t =
    S.H.fold
      (fun k v acc -> match v with Set { v } -> f acc k v | _ -> acc)
      t.h acc

  let filter_map_inplace (f : S.t -> 'a -> 'a option) (t : 'a t) =
    S.H.iter
      (fun k _ -> change (function Some v -> f k v | None -> None) t k)
      t.h

  let clear t = S.H.iter (fun k _ -> remove t k) t.h
  let length t = S.H.length t.h
end

module type HashtblWithoutRemove = sig
  type 'a t
  type key

  val pp : 'a Fmt.t -> 'a t Fmt.t
  val create : creator -> 'a t
  val set : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_def : 'a t -> def:'a -> key -> 'a
  val find_exn : 'a t -> exn -> key -> 'a
  val mem : 'a t -> key -> bool
  val change : ('a option -> 'a) -> 'a t -> key -> unit
  val add_change : ('b -> 'a) -> ('b -> 'a -> 'a) -> 'a t -> key -> 'b -> unit
  val choose : 'a t -> (key * 'a) option
  val iter : f:(key -> 'a -> unit) -> 'a t -> unit
  val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val filter_map_inplace : (key -> 'a -> 'a) -> 'a t -> unit
  val length : 'a t -> int
end

module HashtblWithoutRemove (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  HashtblWithoutRemove with type key := S.t = struct
  type before = B
  type last = L

  let _ = B
  let _ = L

  type (_, _) data =
    | Set : {
        bp : bp;
        next : ('a, 'c) data;
        mutable v : 'a;
      }
        -> ('a, before) data
    | End : ('a, last) data

  type 'a t = {
    (* The table has multiple values for each keys *)
    h : ('a, before) data S.H.t;
    to_remove : S.t Vector.vector;
    mutable bp : bp;
    context : context;
  }

  let restore t size =
    for i = size to Vector.length t.to_remove - 1 do
      let k = Vector.get t.to_remove i in
      let aux : type a. (_, a) data -> (_, before) data option =
       fun next -> match next with End -> None | Set _ -> Some next
      in
      S.H.change
        (function None -> assert false | Some (Set { next }) -> aux next)
        t.h k
    done;
    Vector.truncate t.to_remove size

  let save (t : _ t) =
    let old_bp = t.bp in
    let top_bp = bp t.context in
    if bp_equal t.bp top_bp then ()
    else
      let saved = Vector.size t.to_remove in
      Vector.push top_bp.when_poping (fun () ->
          restore t saved;
          t.bp <- old_bp);
      t.bp <- top_bp

  (** todo print only last binding *)
  let pp pp fmt t =
    let iter f = S.H.iter (fun k r -> match r with Set { v } -> f k v) in
    Colibri2_popop_lib.Pp.(iter2 iter semi arrow S.pp pp) fmt t.h

  let create context =
    {
      h = S.H.create 10;
      context;
      bp = first_bp context;
      to_remove = Vector.create ();
    }

  let find t k =
    let r = S.H.find t.h k in
    match r with Set r -> r.v

  let find_opt t k =
    let open Option in
    let* r = S.H.find_opt t.h k in
    match r with Set r -> Some r.v

  let find_exn t exn k = match S.H.find_exn t.h exn k with Set { v } -> v

  let find_def t ~def k =
    match S.H.find_opt t.h k with None -> def | Some (Set r) -> r.v

  let add_binding t k v next =
    S.H.replace t.h k (Set { v; bp = t.bp; next });
    Vector.push t.to_remove k

  let mem t k = Option.is_some (find_opt t k)

  let set_aux (t : 'a t) k v : ('a, before) data -> unit = function
    | Set r as l -> if bp_equal r.bp t.bp then r.v <- v else add_binding t k v l

  let set t k v =
    save t;
    match S.H.find_opt t.h k with
    | Some s -> set_aux t k v s
    | None -> add_binding t k v End

  let change f (t : 'a t) k =
    let v = f (find_opt t k) in
    set t k v

  let add_change first add (t : 'a t) k v =
    match find_opt t k with
    | None -> set t k (first v)
    | Some v' -> set t k (add v v')

  let choose (type a) (t : a t) =
    let exception Found of (S.t * a) in
    try
      S.H.iter
        (fun k v -> match v with Set { v } -> raise_notrace (Found (k, v)))
        t.h;
      None
    with Found (key, v) -> Some (key, v)

  let iter ~f t = S.H.iter (fun k v -> match v with Set { v } -> f k v) t.h

  let fold f acc t =
    S.H.fold (fun k v acc -> match v with Set { v } -> f acc k v) t.h acc

  let filter_map_inplace (f : S.t -> 'a -> 'a) (t : 'a t) =
    save t;
    S.H.iter (fun k (Set r as s) -> set_aux t k (f k r.v) s) t.h

  let length t = S.H.length t.h
end

module Array = struct
  type 'a t = 'a Ref.t array

  let create ctx i v = Array.init i (fun _ -> Ref.create ctx v)
  let set t i v = Ref.set (Array.get t i) v
  let get t i = Ref.get (Array.get t i)
end

module TimeWheel =
  Colibri2_popop_lib.TimeWheel.Make
    (struct
      type t = creator
    end)
    (Array)
    (Ref)

module type HashtblWithDefault = sig
  type 'a t
  type key

  val create : creator -> (creator -> 'a) -> 'a t
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val set : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val change : ('a -> 'a) -> 'a t -> key -> unit
end

module HashtblWithDefault
    (S : Colibri2_popop_lib.Popop_stdlib.Datatype) : sig
  include HashtblWithDefault

  val find_aux : 'a t -> key -> 'a Ref.t
end
with type key := S.t = struct
  type 'a t = { h : 'a Ref.t S.H.t; def : creator -> 'a; creator : creator }

  let create creator def = { h = S.H.create 5; def; creator }

  let pp pp =
    Fmt.(
      iter_bindings ~sep:comma
        (fun f t -> S.H.iter (fun k v -> f k (Ref.get v)) t.h)
        (pair S.pp pp))

  let find_aux t k =
    match S.H.find_opt t.h k with
    | Some r -> r
    | None ->
        let r = Ref.create t.creator (t.def t.creator) in
        S.H.add t.h k r;
        r

  let find t k = Ref.get (find_aux t k)

  let set t k v =
    let r = find_aux t k in
    Ref.set r v

  let change f t k =
    let r = find_aux t k in
    let v = f (Ref.get r) in
    Ref.set r v
end

module type Memo = sig
  type 'a t
  type key

  val length : 'a t -> int
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val create : creator -> (creator -> key -> 'a) -> 'a t
  val find : 'a t -> key -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
end

module Memo (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  Memo with type key := S.t = struct
  type 'a t = { h : 'a S.H.t; def : creator -> S.t -> 'a; creator : creator }

  let pp pp fmt t =
    let iter f h = S.H.iter (fun k r -> f k r) h.h in
    Colibri2_popop_lib.Pp.(iter2 iter semi arrow S.pp pp) fmt t

  let create creator def = { h = S.H.create 5; def; creator }

  let find t k =
    match S.H.find_opt t.h k with
    | Some r -> r
    | None ->
        let r = t.def t.creator k in
        S.H.add t.h k r;
        r

  let iter f t = S.H.iter f t.h
  let fold f t = S.H.fold f t.h
  let length t = S.H.length t.h
end

module type Clicket = sig
  type 'a t

  val create : creator -> 'a t
  val push : 'a t -> 'a -> unit
  val iter : f:('a -> unit) -> 'a t -> unit
  val todo : 'a t -> bool
  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end

module Clicket = struct
  type 'a t = {
    v : 'a Vector.vector;
    h : int Expert.history;
    mutable first_not_done : int;
  }

  let restore t saved = t.first_not_done <- saved
  let save t = t.first_not_done

  (* let refresh t = Expert.refresh ~restore:(restore t) t.h *)

  let save t = Expert.save ~restore ~save t t.h

  let clear t =
    Vector.clear t.v;
    t.first_not_done <- 0

  let create creator =
    { v = Vector.create (); h = Expert.create creator; first_not_done = 0 }

  let iter ~f t =
    save t;
    for i = t.first_not_done to Vector.length t.v - 1 do
      f (Vector.get t.v i)
    done;
    t.first_not_done <- Vector.length t.v;
    if List.is_empty t.h.context.bps then clear t

  let todo t = t.first_not_done < Vector.length t.v

  let push t v =
    save t;
    Vector.push t.v v

  let pp ?sep pp fmt v = Vector.pp ?pp_sep:sep pp fmt v.v
end

type 'k fold = { fold : 'a. ('a -> 'k -> 'a) -> 'a -> 'a }

module type Trie = sig
  type 'a t
  type key

  val create : creator -> 'a t
  val pp : 'a Fmt.t -> 'a t Fmt.t

  module List : sig
    val set : 'a t -> key list -> 'a -> unit
    val find_def : default:(creator -> 'a) -> 'a t -> key list -> 'a
  end

  module Set : sig
    type set

    val set : 'a t -> set -> 'a -> unit
    val find_def : default:(creator -> 'a) -> 'a t -> set -> 'a
  end

  module Fold : sig
    val set : 'a t -> key fold -> 'a -> unit
    val find_def : default:(creator -> 'a) -> 'a t -> key fold -> 'a
    val memo : default:(creator -> 'a) -> 'a t -> key fold -> 'a
  end
end

module Trie (S : Colibri2_popop_lib.Popop_stdlib.Datatype) :
  Trie with type key := S.t and type Set.set := S.S.t = struct
  module H = HashtblWithDefault (S)

  type 'a node =
    | Empty
    | Value of 'a
    | Node of 'a node H.t
    | NodeValue of 'a * 'a node H.t

  type 'a t = 'a node Ref.t

  let create c = Ref.create c Empty

  let pp pp fmt t =
    let rec aux fmt c =
      match c with
      | Empty -> ()
      | Value v -> pp fmt v
      | Node h -> H.pp aux fmt h
      | NodeValue (v, h) ->
          pp fmt v;
          H.pp aux fmt h
    in
    aux fmt (Ref.get t)

  module Fold = struct
    let find_aux t { fold } =
      let aux acc k =
        match Ref.get acc with
        | Empty ->
            let h = H.create (Ref.creator acc) (fun _ -> Empty) in
            Ref.set acc (Node h);
            H.find_aux h k
        | Value x ->
            let h = H.create (Ref.creator acc) (fun _ -> Empty) in
            Ref.set acc (NodeValue (x, h));
            H.find_aux h k
        | Node h | NodeValue (_, h) -> H.find_aux h k
      in
      let acc = fold aux t in
      acc
    [@@inline]

    let set t fold v =
      let r = find_aux t fold in
      match Ref.get r with
      | Empty -> Ref.set r (Value v)
      | Value v' -> if Base.phys_equal v v' then Ref.set r (Value v)
      | Node h | NodeValue (_, h) -> Ref.set r (NodeValue (v, h))
    [@@inline]

    let find_def ~default t fold =
      let r = find_aux t fold in
      match Ref.get r with
      | Empty | Node _ -> default (Ref.creator r)
      | Value v | NodeValue (v, _) -> v
    [@@inline]

    let memo ~default t fold =
      let r = find_aux t fold in
      match Ref.get r with
      | Empty ->
          let v = default (Ref.creator r) in
          Ref.set r (Value v);
          v
      | Node h ->
          let v = default (Ref.creator r) in
          Ref.set r (NodeValue (v, h));
          v
      | Value v | NodeValue (v, _) -> v
    [@@inline]
  end

  module List = struct
    let set t l v =
      Fold.set t { fold = (fun f acc -> List.fold_left f acc l) } v

    let find_def ~default t l =
      Fold.find_def ~default t { fold = (fun f acc -> List.fold_left f acc l) }
  end

  module Set = struct
    let set t l v = Fold.set t { fold = (fun f acc -> S.S.fold_left f acc l) } v

    let find_def ~default t l =
      Fold.find_def ~default t { fold = (fun f acc -> S.S.fold_left f acc l) }
  end
end
