(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

(* Disclaimer
   ----------
   This module uses very unsafe caml features (module Obj).
   Modify it at your own risk.
   Sometimes the caml type system does not help you here.
   Introducing a bug here may introduce some "segmentation faults" in Frama-C *)

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

type concrete_repr =
  { mutable name: string;
    digest: Digest.t;
    (* structural_descr: Structural_descr.t *)
  }

(* phantom type *)
type 'a t = concrete_repr
type 'a ty = 'a t

(* non-phantom type: the type variable is used here *)
type 'a full_t = { ty: 'a t; reprs: 'a list }

(* ****************************************************************************)
(** {2 Type values are comparable} *)
(* ****************************************************************************)

module Comparable = struct
  let equal x y = x.digest = y.digest
  let compare x y = String.compare x.digest y.digest
  let hash x = Hashtbl.hash x.digest
end
include Comparable

module Tbl = Hashtbl.Make(struct type t = concrete_repr include Comparable end)

(* ****************************************************************************)
(** {2 Global useful values} *)
(* ****************************************************************************)

let types : (string (* name *), Obj.t full_t) Hashtbl.t = Hashtbl.create 97
let embedded_types: concrete_repr Tbl.t = Tbl.create 7

(* ****************************************************************************)
(* ****************************************************************************)

(* exception AlreadyExists of string *)

(* let register ?(closure=false) ~name structural_descr reprs = *)
(*   let error () = *)
(*     invalid_arg ("Type.register: invalid reprs for type " ^ name) *)
(*   in *)
(*   (1*  Format.printf "type %S@." name;*)
(*   match reprs with *)
(*   | [] -> error () *)
(*   | r :: _ when Obj.tag (Obj.repr r) = Obj.closure_tag && not closure -> *)
(*     (1* all the representants have the same types: *)
(*        thus that is correct to check only the first one *1) *)
(*     error () *)
(*   | _ -> *)
(*     let digest = match structural_descr with *)
(*       | Structural_descr.Unknown -> *)
(*         (1* unserializable type: weakest digest *1) *)
(*         Digest.string name *)
(*       | _ -> *)
(*         let key = name, Structural_descr.cleanup structural_descr, reprs in *)
(*         Digest.string (Marshal.to_string key []) *)
(*     in *)
(*     let ty = *)
(*       { name = name; *)
(*         digest = digest; *)
(*         structural_descr = structural_descr } *)
(*     in *)
(*     match Hashtbl.find_opt types name with *)
(*     (1* Either the type is already registered *1) *)
(*     | Some full_ty -> *)
(*       (1* then check that the new type is the same as the old one *1) *)
(*       if equal ty full_ty.ty *)
(*       then full_ty.ty *)
(*       else raise (AlreadyExists name) *)
(*     (1* or this is a brand new type *1) *)
(*     | None -> *)
(*       let full_ty = { ty = ty; reprs = List.map Obj.repr reprs } in *)
(*       Hashtbl.add types name full_ty; *)
(*       ty *)

let add_abstract_types = ref (fun _ _ -> ())

exception No_abstract_type of string

module Abstract(T: sig val name: string end) = struct
  type t
  let ty =
    try (Hashtbl.find types T.name).ty
    with Not_found -> raise (No_abstract_type T.name)

  let () =
    let p = match Str.split (Str.regexp_string ".") T.name with
      | [] ->
        failwith "name as argument of `Type.Abstract' must be a valid OCaml \
                  type name"
      | p :: _ -> p
    in
    !add_abstract_types p T.name
end

let name ty = ty.name
(* let structural_descr ty = ty.structural_descr *)
let digest ty = ty.digest

let unsafe_reprs ty = (Hashtbl.find types ty.name).reprs
let reprs ty =
  let l = try unsafe_reprs ty with Not_found -> assert false in
  List.map Obj.obj l

let set_name ty name =
  let full_ty = try Hashtbl.find types ty.name with Not_found -> assert false in
  Hashtbl.remove types ty.name;
  ty.name <- name;
  Hashtbl.add types name full_ty

let rec get_embedded_type_names ty =
  let sub_ty = try Tbl.find_all embedded_types ty with Not_found -> [] in
  let sub_ty_names =
    List.fold_left (fun acc ty -> get_embedded_type_names ty @ acc) [] sub_ty
  in
  ty.name :: sub_ty_names

(* ****************************************************************************)
(** {2 Polymorphic type values} *)
(* ****************************************************************************)

module type Polymorphic_input = sig
  val name: 'a t -> string
  (* val structural_descr: Structural_descr.t -> Structural_descr.t *)
  type 'a t
  (* val reprs: 'a -> 'a t list *)
end

module type Polymorphic = sig
  type 'a poly
  (* val instantiate: 'a t -> 'a poly t * bool *)
  val is_instance_of: 'a t -> bool
  val get_instance: 'a poly t -> 'a t
end

module Polymorphic(T: Polymorphic_input) = struct

  module Tbl = struct
    let memo : concrete_repr Tbl.t = Tbl.create 17
    let instances: concrete_repr Tbl.t = Tbl.create 17

    let add instance ty =
      Tbl.add memo instance ty;
      Tbl.add instances ty instance;
      Tbl.add embedded_types ty instance

    let find = Tbl.find memo
    let find_instance = Tbl.find instances
    let mem_instance = Tbl.mem memo
  end

  type 'a poly = 'a T.t

  (* let instantiate (ty:'a t) = *)
  (*   try *)
  (*     Tbl.find ty, false *)
  (*   with Not_found -> *)
  (*     let repr = *)
  (*       register *)
  (*         ~name:(T.name ty) *)
  (*         (T.structural_descr ty.structural_descr) *)
  (*         (List.fold_left *)
  (*            (fun acc ty -> T.reprs ty @ acc) [] (unsafe_reprs ty)) *)
  (*     in *)
  (*     Tbl.add ty repr; *)
  (*     repr, true *)

  let is_instance_of = Tbl.mem_instance

  let get_instance (type a) (ty:a poly t) =
    try
      Tbl.find_instance ty
    with Not_found ->
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false

end

(* module type Polymorphic2_input = sig *)
(*   val name: 'a t -> 'b t -> string *)
(*   val structural_descr: *)
(*     Structural_descr.t -> Structural_descr.t -> Structural_descr.t *)
(*   type ('a, 'b) t *)
(*   val reprs: 'a -> 'b -> ('a, 'b) t list *)
(* end *)

(* module type Polymorphic2 = sig *)
(*   type ('a, 'b) poly *)
(*   val instantiate: 'a t -> 'b t -> ('a, 'b) poly t * bool *)
(*   val is_instance_of: 'a t -> bool *)
(*   val get_instance: ('a, 'b) poly t -> 'a t * 'b t *)
(* end *)

module Concrete_pair =
  Hashtbl.Make
    (struct
      type t = concrete_repr * concrete_repr
      let hash (x,y) = Hashtbl.hash (hash x, hash y)
      let equal (x1,y1) (x2,y2) = equal x1 x2 && equal y1 y2
    end)

(* module Polymorphic2(T: Polymorphic2_input) = struct *)

(*   type ('a, 'b) poly = ('a, 'b) T.t *)

(*   let memo_tbl : concrete_repr Concrete_pair.t = Concrete_pair.create 17 *)
(*   let instances : (concrete_repr * concrete_repr) Tbl.t = Tbl.create 17 *)

(*   let instantiate a b = *)
(*     let key = a, b in *)
(*     try *)
(*       Concrete_pair.find memo_tbl key, false *)
(*     with Not_found -> *)
(*       let reprs = *)
(*         List.fold_left *)
(*           (fun acc r1 -> *)
(*              List.fold_left *)
(*                (fun acc r2 -> T.reprs r1 r2 @ acc) *)
(*                acc *)
(*                (unsafe_reprs b)) *)
(*           [] *)
(*           (unsafe_reprs a) *)
(*       in *)
(*       let ty = *)
(*         register *)
(*           ~name:(T.name a b) *)
(*           (T.structural_descr a.structural_descr b.structural_descr) *)
(*           reprs *)
(*       in *)
(*       Concrete_pair.add memo_tbl key ty; *)
(*       Tbl.add instances ty key; *)
(*       Tbl.add embedded_types ty a; *)
(*       Tbl.add embedded_types ty b; *)
(*       ty, true *)

(*   let is_instance_of ty = Tbl.mem instances ty *)

(*   let get_instance (type a) (type b) (ty:(a, b) poly t) = *)
(*     try *)
(*       Tbl.find instances ty *)
(*     with Not_found -> *)
(*       (1* static typing ensures than [ty] has already been instantiated. *1) *)
(*       assert false *)

(* end *)

(* ****************************************************************************)
(** {2 Functional types} *)
(* ****************************************************************************)

let par_ty_name test ty =
  if test ty then Format.sprintf "(%s)" ty.name
  else Format.sprintf "%s" ty.name

(* module Function = struct *)

(*   type ('a, 'b) poly = 'a -> 'b *)

(*   type instance = *)
(*     { arg: concrete_repr; ret: concrete_repr; label: string option } *)

(*   module Memo = *)
(*     Hashtbl.Make *)
(*       (struct *)
(*         type t = instance *)
(*         let hash x = Hashtbl.hash (hash x.arg, hash x.ret, x.label) *)
(*         let equal x y = *)
(*           equal x.arg y.arg && equal x.ret y.ret && x.label = y.label *)
(*       end) *)
(*   let memo_tbl : concrete_repr Memo.t = Memo.create 17 *)
(*   let instances *)
(*     : (instance * Obj.t (1* default value of the optional label *1) option) *)
(*         Tbl.t *)
(*     = Tbl.create 17 *)

(*   let is_instance_of ty = Tbl.mem instances ty *)

(*   let get_instance (type a) (type b) (ty:(a, b) poly t) = *)
(*     try *)
(*       let instance, _ = Tbl.find instances ty in *)
(*       instance.arg, instance.ret, instance.label *)
(*     with Not_found -> *)
(*       (1* static typing ensures than [ty] has already been instantiated. *1) *)
(*       assert false *)

(*   let get_optional_argument (type a) (type b) (ty:(a, b) poly t) = *)
(*     try *)
(*       match Tbl.find instances ty with *)
(*       | _, None -> None *)
(*       | _, Some o -> Some (Obj.obj o : unit -> 'b) *)
(*     with Not_found -> *)
(*       (1* static typing ensures than [ty] has already been instantiated. *1) *)
(*       assert false *)

(*   let name label ty1 ty2 = *)
(*     (match label with None -> "" | Some l -> "~" ^ l ^ ":") *)
(*     ^ par_ty_name is_instance_of ty1 ^ " -> " ^ name ty2 *)

(*   let instantiate ?label (a:'a) (b:'b t): ('a, 'b) poly t * bool = *)
(*     let l, o = match label with *)
(*       | None -> None, None *)
(*       | Some (l, None) -> Some l, None *)
(*       | Some (l, Some o) -> Some l , Some (Obj.repr o) *)
(*     in *)
(*     let key = { arg = a; ret = b; label = l } in *)
(*     try *)
(*       Memo.find memo_tbl key, false *)
(*     with Not_found -> *)
(*       let ty = *)
(*         (1* Do not inline [Types.repr b] in the closure below because *)
(*            caml is not able to marshal the closure. *)
(*            Sadly don't know exactly why. Seem to have some value tagged as *)
(*            abstract in the closure environment. *1) *)
(*         register *)
(*           ~closure:true *)
(*           ~name:(name l a b) *)
(*           Structural_descr.t_unknown *)
(*           (List.map (fun r _ -> r) (unsafe_reprs b)) *)
(*       in *)
(*       Memo.add memo_tbl key ty; *)
(*       Tbl.add instances ty (key, o); *)
(*       Tbl.add embedded_types ty a; *)
(*       Tbl.add embedded_types ty b; *)
(*       ty, true *)

(* end *)

(* ****************************************************************************)
(** {2 Polymorphic3} *)
(* ****************************************************************************)

(* module type Polymorphic3_input = sig *)
(*   val name: 'a t -> 'b t -> 'c t -> string *)
(*   val structural_descr: *)
(*     Structural_descr.t -> Structural_descr.t -> Structural_descr.t -> *)
(*     Structural_descr.t *)
(*   type ('a, 'b, 'c) t *)
(*   val reprs: 'a -> 'b -> 'c -> ('a, 'b, 'c) t list *)
(* end *)

(* module type Polymorphic3 = sig *)
(*   type ('a, 'b, 'c) poly *)
(*   val instantiate: 'a t -> 'b t -> 'c t -> ('a, 'b, 'c) poly t * bool *)
(*   val is_instance_of: 'a t -> bool *)
(*   val get_instance: ('a, 'b, 'c) poly t -> 'a t * 'b t * 'c t *)
(* end *)

(* module Concrete_triple = *)
(*   Hashtbl.Make *)
(*     (struct *)
(*       type t = concrete_repr * concrete_repr * concrete_repr *)
(*       let hash (x,y,z) = Hashtbl.hash (hash x, hash y, hash z) *)
(*       let equal (x1,y1,z1) (x2,y2,z2) = *)
(*         equal x1 x2 && equal y1 y2 && equal z1 z2 *)
(*     end) *)

(* module Polymorphic3(T:Polymorphic3_input) = struct *)

(*   type ('a, 'b, 'c) poly = ('a, 'b, 'c) T.t *)

(*   let memo_tbl: concrete_repr Concrete_triple.t = Concrete_triple.create 17 *)
(*   let instances *)
(*     : (concrete_repr * concrete_repr * concrete_repr) Tbl.t *)
(*     = Tbl.create 17 *)

(*   let instantiate a b c = *)
(*     let key = a, b, c in *)
(*     try *)
(*       Concrete_triple.find memo_tbl key, false *)
(*     with Not_found -> *)
(*       let reprs = *)
(*         List.fold_left *)
(*           (fun acc r1 -> *)
(*              List.fold_left *)
(*                (fun acc r2 -> *)
(*                   List.fold_left *)
(*                     (fun acc r3 -> T.reprs r1 r2 r3 @ acc) *)
(*                     acc *)
(*                     (unsafe_reprs c)) *)
(*                acc *)
(*                (unsafe_reprs b)) *)
(*           [] *)
(*           (unsafe_reprs a) *)
(*       in *)
(*       let ty = *)
(*         register *)
(*           ~name:(T.name a b c) *)
(*           (T.structural_descr *)
(*              a.structural_descr *)
(*              b.structural_descr *)
(*              c.structural_descr) *)
(*           reprs *)
(*       in *)
(*       Concrete_triple.add memo_tbl key ty; *)
(*       Tbl.add instances ty key; *)
(*       Tbl.add embedded_types ty a; *)
(*       Tbl.add embedded_types ty b; *)
(*       Tbl.add embedded_types ty c; *)
(*       ty, true *)

(*   let is_instance_of ty = Tbl.mem instances ty *)

(*   let get_instance (type a) (type b) (type c) (ty:(a, b, c) poly t) = *)
(*     try *)
(*       Tbl.find instances ty *)
(*     with Not_found -> *)
(*       (1* static typing ensures than [ty] has already been instantiated. *1) *)
(*       assert false *)

(* end *)

(* ****************************************************************************)
(** {2 Polymorphic4} *)
(* ****************************************************************************)

(* module type Polymorphic4_input = sig *)
(*   val name: 'a t -> 'b t -> 'c t -> 'd t -> string *)
(*   val structural_descr: *)
(*     Structural_descr.t -> Structural_descr.t -> Structural_descr.t -> *)
(*     Structural_descr.t -> Structural_descr.t *)
(*   type ('a, 'b, 'c, 'd) t *)
(*   val reprs: 'a -> 'b -> 'c -> 'd -> ('a, 'b, 'c, 'd) t list *)
(* end *)

(* module type Polymorphic4 = sig *)
(*   type ('a, 'b, 'c, 'd) poly *)
(*   val instantiate: *)
(*     'a t -> 'b t -> 'c t -> 'd t -> ('a, 'b, 'c, 'd) poly t * bool *)
(*   val is_instance_of: 'a t -> bool *)
(*   val get_instance: ('a, 'b, 'c, 'd) poly t -> 'a t * 'b t * 'c t * 'd t *)
(* end *)

(* module Concrete_quadruple = *)
(*   Hashtbl.Make *)
(*     (struct *)
(*       type t = concrete_repr * concrete_repr * concrete_repr * concrete_repr *)
(*       let hash (x,y,z,t) = Hashtbl.hash (hash x, hash y, hash z, hash t) *)
(*       let equal (x1,y1,z1,t1) (x2,y2,z2,t2) = *)
(*         equal x1 x2 && equal y1 y2 && equal z1 z2 && equal t1 t2 *)
(*     end) *)

(* module Polymorphic4(T:Polymorphic4_input) = struct *)

(*   type ('a, 'b, 'c, 'd) poly = ('a, 'b, 'c, 'd) T.t *)

(*   let memo_tbl *)
(*     : concrete_repr Concrete_quadruple.t *)
(*     = Concrete_quadruple.create 17 *)

(*   let instances *)
(*     : (concrete_repr * concrete_repr * concrete_repr * concrete_repr) Tbl.t *)
(*     = Tbl.create 17 *)

(*   let instantiate a b c d = *)
(*     let key = a, b, c, d in *)
(*     try *)
(*       Concrete_quadruple.find memo_tbl key, false *)
(*     with Not_found -> *)
(*       let reprs = *)
(*         List.fold_left *)
(*           (fun acc r1 -> *)
(*              List.fold_left *)
(*                (fun acc r2 -> *)
(*                   List.fold_left *)
(*                     (fun acc r3 -> *)
(*                        List.fold_left *)
(*                          (fun acc r4 -> T.reprs r1 r2 r3 r4 @ acc) *)
(*                          acc *)
(*                          (unsafe_reprs d)) *)
(*                     acc *)
(*                     (unsafe_reprs c)) *)
(*                acc *)
(*                (unsafe_reprs b)) *)
(*           [] *)
(*           (unsafe_reprs a) *)
(*       in *)
(*       let ty = *)
(*         register *)
(*           ~name:(T.name a b c d) *)
(*           (T.structural_descr *)
(*              a.structural_descr *)
(*              b.structural_descr *)
(*              c.structural_descr *)
(*              d.structural_descr) *)
(*           reprs *)
(*       in *)
(*       Concrete_quadruple.add memo_tbl key ty; *)
(*       Tbl.add instances ty key; *)
(*       Tbl.add embedded_types ty a; *)
(*       Tbl.add embedded_types ty b; *)
(*       Tbl.add embedded_types ty c; *)
(*       Tbl.add embedded_types ty d; *)
(*       ty, true *)

(*   let is_instance_of ty = Tbl.mem instances ty *)

(*   let get_instance *)
(*       (type a) (type b) (type c) (type d) (ty:(a, b, c, d) poly t) = *)
(*     try *)
(*       Tbl.find instances ty *)
(*     with Not_found -> *)
(*       (1* static typing ensures than [ty] has already been instantiated. *1) *)
(*       assert false *)

(* end *)

(* ****************************************************************************)
(** {2 Heterogeneous Tables} *)
(* ****************************************************************************)

(* module Ty_tbl(Info: sig type 'a t end) = struct *)
(*   type t = Obj.t Tbl.t *)
(*   let create x = Tbl.create x *)
(*   let add (type a) tbl (ty:a ty) (x:a Info.t) = Tbl.add tbl ty (Obj.repr x) *)
(*   let find (type a) tbl (ty:a ty) = (Obj.obj (Tbl.find tbl ty) : a Info.t) *)
(* end *)

(* module Obj_tbl: sig *)
(*   type 'a t *)
(*   val create: unit -> 'a t *)
(*   val add: 'a t -> 'b ty -> 'b -> 'a -> unit *)
(*   val find: 'a t -> 'b ty -> 'b -> 'a *)
(*   val mem: 'a t -> 'b ty -> 'b -> bool *)
(*   val iter: 'b t -> ('a ty -> 'a -> 'b -> unit) -> unit *)
(* end = struct *)

(*   module O = *)
(*     Hashtbl.Make(struct *)
(*       type t = Obj.t *)
(*       let equal = (==) *)
(*       let hash x = *)
(*         (1* 0 is correct; trying to do a bit better... *1) *)
(*         let tag = Obj.tag x in *)
(*         if tag = 0 then *)
(*           0 *)
(*         else if tag = Obj.closure_tag then *)
(*           (1* Buggy code with OCaml 4.01, deactivated for now *)
(*              (1* assumes that the first word of a closure does not change in *)
(*              any way (even by Gc.compact invocation). *1) *)
(*              Obj.magic (Obj.field x 0)*) 
(*           (1* to be tested (suggested by Damien D.): add a 'xor 0' *1) *)
(*           (1*         Obj.magic (Obj.field x 0)*) 
(*           0 *)
(*         else *)
(*           Hashtbl.hash x *)
(*     end) *)

(*   type 'a t = 'a O.t Tbl.t *)

(*   let create () = Tbl.create 7 *)

(*   let add tbl ty k v = *)
(*     let tytbl = *)
(*       try Tbl.find tbl ty *)
(*       with Not_found -> *)
(*         let tytbl = O.create 7 in *)
(*         Tbl.add tbl ty tytbl; *)
(*         tytbl *)
(*     in *)
(*     O.replace tytbl (Obj.repr k) v *)

(*   let find tbl ty k = *)
(*     O.find (Tbl.find tbl ty) (Obj.repr k) *)

(*   let mem tbl ty k = *)
(*     try *)
(*       let objs = Tbl.find tbl ty in *)
(*       O.mem objs (Obj.repr k) *)
(*     with Not_found -> *)
(*       false *)

(*   let iter tbl f = *)
(*     Tbl.iter (fun ty objs -> O.iter (fun o v -> f ty (Obj.obj o) v) objs) tbl *)

(* end *)

(* module type Heterogeneous_table = sig *)
(*   type key *)
(*   type 'a info *)
(*   type t *)
(*   val create: int -> t *)
(*   val add: t -> key -> 'a ty -> 'a info -> unit *)
(*   exception Unbound_value of string *)
(*   exception Incompatible_type of string *)
(*   val find: t -> key -> 'a ty -> 'a info *)
(*   val iter: (key -> 'a ty -> 'a info -> unit) -> t -> unit *)
(*   val fold: (key -> 'a ty -> 'a info -> 'b -> 'b) -> t -> 'b -> 'b *)
(* end *)

(* module Make_tbl *)
(*     (Key: sig include Hashtbl.HashedType val to_string: t -> string end) *)
(*     (Info: sig type 'a t end) = *)
(* struct *)

(*   type key = Key.t *)
(*   type 'a info = 'a Info.t *)
(*   type data = { ty: concrete_repr; o: Obj.t } *)
(*   module H = Hashtbl.Make(Key) *)
(*   type t = data H.t *)

(*   exception Incompatible_type of string *)

(*   let create x = H.create x *)

(*   let add tbl s ty x = *)
(*     let name = Key.to_string s in *)
(*     if H.mem tbl s then raise (AlreadyExists name); *)
(*     H.add tbl s { ty = ty; o = Obj.repr x } *)

(*   exception Unbound_value of string *)
(*   let type_error s ty_name ty_name' = *)
(*     raise *)
(*       (Incompatible_type *)
(*          (Format.sprintf "%s has type %s but is used with type %s" *)
(*             s ty_name' ty_name)) *)

(*   let find tbl s ty = *)
(*     let name = Key.to_string s in *)
(*     let data = *)
(*       try H.find tbl s with Not_found -> raise (Unbound_value name) *)
(*     in *)
(*     if ty.digest <> data.ty.digest then *)
(*       type_error name ty.name data.ty.name; *)
(*     Obj.obj data.o *)

(*   let iter f tbl = *)
(*     H.iter (fun k v -> f k v.ty (Obj.obj v.o)) tbl *)

(*   let fold f tbl acc = *)
(*     H.fold (fun k v acc -> f k v.ty (Obj.obj v.o) acc) tbl acc *)

(* end *)

(* module String_tbl = *)
(*   Make_tbl *)
(*     (struct *)
(*       type t = string *)
(*       let hash x = Hashtbl.hash x *)
(*       let equal : string -> string -> bool = (=) *)
(*       let to_string x = x *)
(*     end) *)
