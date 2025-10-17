(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ********************************************************************** *)
(** {2 Type declarations} *)
(* ********************************************************************** *)

type 'a t =
  { equal: 'a -> 'a -> bool;
    compare: 'a -> 'a -> int;
    hash: 'a -> int;
    copy: 'a -> 'a;
    pretty: Format.formatter -> 'a -> unit;
    (* mem_project: (Project_skeleton.t -> bool) -> 'a -> bool *) 
}

type 'a info = 'a t

module type Ty = sig
  type t
  (* val ty: t Type.t *)
end

module type S_no_copy = sig
  include Ty
  val datatype_name: string
  (* val datatype_descr: t Descr.t *)
  (* val packed_descr: Structural_descr.pack *)
  (* val reprs: t list *)
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  (* val pretty: Format.formatter -> t -> unit *)
  (* val mem_project: (Project_skeleton.t -> bool) -> t -> bool *)
end

module type S = sig
  include S_no_copy
  val copy: t -> t
end

(* ********************************************************************** *)
(** {2 Getters from a type value} *)
(* ********************************************************************** *)

(* module Infos = Type.Ty_tbl(struct type 'a t = 'a info end) *)

(* let info_tbl = Infos.create 97 *)

(* let internal_info s ty = *)
(*   try Infos.find info_tbl ty *)
(*   with Not_found -> *)
(*     Format.eprintf "Internal Datatype.info error: no %s for %S@." *)
(*       s (Type.name ty); *)
(*     assert false *)

(* let equal ty = (internal_info "equal" ty).equal *)
(* let compare ty = (internal_info "compare" ty).compare *)
(* let hash ty = (internal_info "hash" ty).hash *)
(* let copy ty = (internal_info "copy" ty).copy *)
(* let pretty ty = (internal_info "pretty" ty).pretty *)
(* let mem_project ty = (internal_info "mem_project" ty).mem_project *)

(* let info ty = internal_info "info" ty *)

(* ********************************************************************** *)
(** {2 Easy builders} *)
(* ********************************************************************** *)

let undefined _ = assert false
let identity x = x
let never_any_project _ _ = false
let from_compare _ _ = assert false

module type Undefined = sig
  (* val structural_descr: Structural_descr.t *)
  val equal: 'a -> 'a -> bool
  val compare: 'a -> 'a -> int
  val hash: 'a -> int
  val rehash: 'a -> 'a
  val copy: 'a -> 'a
  (* val pretty: Format.formatter -> 'a -> unit *)
  (* val mem_project: (Project_skeleton.t -> bool) -> 'a -> bool *)
end

module Partial_undefined = struct
  let equal = undefined
  let compare = undefined
  let hash = undefined
  let copy = undefined
  let pretty = undefined
  let mem_project = undefined
end

module Undefined = struct
  include Partial_undefined
  (* let structural_descr = Structural_descr.t_unknown *)
  let rehash = undefined
end

module Serializable_undefined = struct
  include Partial_undefined
  (* let structural_descr = Structural_descr.t_abstract *)
  let rehash = identity
  let mem_project = never_any_project
end

(* ********************************************************************** *)
(** {2 Generic builders} *)
(* ********************************************************************** *)

let check f fname tname =
  assert
    (if f == undefined then begin
        Format.printf "@[Preliminary datatype check failed.@\n\
                       Value `%s' of type %s is required.@]@."
          fname tname;
        false
      end else
       true)

module Build
    (T: sig
       type t
       (* val ty: t Type.t *)
       (* val reprs: t list *)
       val equal: t -> t -> bool
       val compare: t -> t -> int
       val hash: t -> int
       val rehash: t -> t
       val copy: t -> t
       (* val pretty: Format.formatter -> t -> unit *)
       (* val mem_project: (Project_skeleton.t -> bool) -> t -> bool *)
     end) =
struct

  let datatype_name = "bob"

  let equal =
    if T.equal == from_compare then (fun x y -> T.compare x y = 0)
    else T.equal

  let compare = T.compare
  let hash = T.hash
  let rehash = T.rehash
  let copy = T.copy
  (* let pretty = T.pretty *)
  (* let mem_project = T.mem_project *)

  (* let info = *)
  (*   { equal = equal; *)
  (*     compare = compare; *)
  (*     hash = hash; *)
  (*     copy = copy; *)
      (* pretty = pretty; *)
      (* mem_project = mem_project *) 
(* } *)

  (* let () = Infos.add info_tbl T.ty info *)

  (* let mk_full_descr d = *)
  (*   let descr = *)
  (*     if Descr.is_unmarshable d then Descr.unmarshable *)
  (*     else *)
  (*     if rehash == undefined then begin *)
  (*       check rehash "rehash" datatype_name; *)
  (*       assert false *)
  (*     end *)
  (*     else *)
  (*     if rehash == identity then d *)
  (*     else *)
  (*       begin *)
  (*         if Descr.is_unmarshable d then begin *)
  (*           check undefined "structural_descr" datatype_name; *)
  (*           assert false *)
  (*         end; *)
  (*         Descr.transform d rehash *)
  (*       end *)
  (*   in *)
  (*   descr, Descr.pack descr *)

  (* let datatype_descr, packed_descr = mk_full_descr (Descr.of_type T.ty) *)
  (* let reprs = T.reprs (1* [Type.reprs] is not usable in the "no-obj" mode *1) *)

  (* let %test_unit _ = *)
  (*   if pretty != undefined then (1* Test defined pretty functions *1) *)
  (*     at_exit (1* Do not test now as some pretty printers are not yet defined *1) *)
  (*       (fun () -> *)
  (*          List.iter (pretty Format.str_formatter) reprs; *)
  (*          ignore (Format.flush_str_formatter ())) *)
end

module type Make_input = sig
  type t
  val name: string
  val rehash: t -> t
  (* val structural_descr: Structural_descr.t *)
  (* val reprs: t list *)
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val copy: t -> t
  (* val pretty: Format.formatter -> t -> unit *)
  (* val mem_project: (Project_skeleton.t -> bool) -> t -> bool *)
end

(* let is_module_name s = *)
(*   let l = Str.split (Str.regexp "\\.") s in *)
(*   List.for_all *)
(*     (fun x -> *)
(*        String.length x > 0 && *)
(*        x.[0] = Char.uppercase_ascii x.[0]) l *)

module Make(X: Make_input) = struct

  module T = struct
    include X
    (* let ty = *)
    (*   let name = if is_module_name X.name then X.name ^ ".t" else X.name in *)
    (*   Type.register ~name X.structural_descr X.reprs *)
  end

  include T
  include Build(T)

end

module type Set = sig
  include Set.S
  val nearest_elt_le: elt -> t -> elt
  val nearest_elt_ge: elt -> t -> elt
  include S with type t := t
end

module type Map = sig
  include Map.S
  module Key: S with type t = key
  module Make(Data: S) : S with type t = Data.t t
end

module type Hashtbl_with_descr = sig
  include Hashtbl.S
  (* val structural_descr: Structural_descr.t -> Structural_descr.t *)
end

module Fc_hashtbl = Hashtbl

module type Hashtbl = sig
  include Hashtbl_with_descr

  (* val make_type: 'a Type.t -> 'a t Type.t *)
  (** @since Fluorine-20130401 *)

  module Key: S with type t = key
  module Make(Data: S) : S with type t = Data.t t
end

module type S_with_set_and_map = sig
  include S
  module Set: Set with type elt = t
  module Map: Map with type key = t
end

module type S_with_hashtbl = sig
  include S
  module Hashtbl: Hashtbl with type key = t
end

module type S_with_collections = sig
  include S
  module Set: Set with type elt = t
  module Map: Map with type key = t
  module Hashtbl: Hashtbl with type key = t
end

(* ****************************************************************************)
(** {2 Polymorphic signature} *)
(* ****************************************************************************)

(* module type Polymorphic = sig *)
(*   include Type.Polymorphic *)
(*   module Make(T: S) : S with type t = T.t poly *)
(* end *)

(* ****************************************************************************)
(** {2 Polymorphic2 } *)
(* ****************************************************************************)

(*module type Polymorphic2 = sig *)
(*  include Type.Polymorphic2 *)
(*  module Make(T1: S)(T2: S) : S with type t = (T1.t, T2.t) poly *)
(*end *)

(*module type Polymorphic2_input = sig *)
(*  include Type.Polymorphic2_input *)
(*  val mk_equal: *)
(*    ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> *)
(*    bool *)
(*  val mk_compare: *)
(*    ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int *)
(*  val mk_hash: ('a -> int) -> ('b -> int) -> ('a, 'b) t -> int *)
(*  val map: ('a -> 'a) -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t *)
(*  val mk_pretty: *)
(*    (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> *)
(*    Format.formatter -> ('a, 'b) t -> unit *)
(*  val mk_mem_project: *)
(*    ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*    ((Project_skeleton.t -> bool) -> 'b -> bool) -> *)
(*    (Project_skeleton.t -> bool) -> ('a, 'b) t -> bool *)
(*end *)

(*module Polymorphic2(P: Polymorphic2_input) = struct *)

(*  include Type.Polymorphic2(P) *)

(*  module Make(T1: S)(T2: S) = struct *)

(*    module T = struct *)
(*      type t = (T1.t, T2.t) P.t *)
(*      let ty, _is_new = instantiate T1.ty T2.ty *)
(*    end *)

(*    include T *)
(*    include Build *)
(*        (struct *)
(*          include T *)
(*          let reprs = Type.reprs ty *)
(*          let build mk f1 f2 = *)
(*            if mk == undefined || f1 == undefined || f2 == undefined then *)
(*              undefined *)
(*            else *)
(*              mk f1 f2 *)
(*          let compare = build P.mk_compare T1.compare T2.compare *)
(*          let equal = build P.mk_equal T1.equal T2.equal *)
(*          let hash = build P.mk_hash T1.hash T2.hash *)
(*          let rehash = identity *)
(*          let copy = *)
(*            let mk f1 f2 = *)
(*              if P.map == undefined then undefined *)
(*              else *)
(*                (1* [JS 2011/05/31] No optimisation for the special case of identity, *)
(*                   since we really want to perform a DEEP copy. *1) *)
(*                (1*if f1 == identity && f2 == identity then identity *)
(*                  else*) (* P.map f1 f2 *)
(*            in *)
(*            build mk T1.copy T2.copy *)
(*          let pretty = build P.mk_pretty T1.pretty T2.pretty *)
(*          let mem_project = *)
(*            let mk f1 f2 = *)
(*              if P.mk_mem_project == undefined then undefined *)
(*              else if f1 == never_any_project && f2 == never_any_project then *)
(*                never_any_project *)
(*              else *)
(*                P.mk_mem_project f1 f2 *)
(*            in *)
(*            build mk T1.mem_project T2.mem_project *)
(*        end) *)

(*    let datatype_descr, packed_descr = *)
(*      mk_full_descr *)
(*        (Descr.of_structural *)
(*           ty *)
(*           (P.structural_descr *)
(*              (Descr.str T1.datatype_descr) (Descr.str T2.datatype_descr))) *)

(*  end *)

(*end *)

(* ****************************************************************************)
(** {2 Polymorphic3 } *)
(* ****************************************************************************)

(*module type Polymorphic3 = sig *)
(*  include Type.Polymorphic3 *)
(*  module Make(T1:S)(T2:S)(T3:S) : S with type t = (T1.t, T2.t, T3.t) poly *)
(*end *)

(*module Polymorphic3 *)
(*    (P: sig *)
(*       include Type.Polymorphic3_input *)
(*       val mk_equal: *)
(*         ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('c -> 'c -> bool) -> *)
(*         ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> *)
(*         bool *)
(*       val mk_compare: *)
(*         ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('c -> 'c -> int) -> *)
(*         ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int *)
(*       val mk_hash: *)
(*         ('a -> int) -> ('b -> int) -> ('c -> int) -> ('a, 'b, 'c) t -> int *)
(*       val map: *)
(*         ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('a, 'c, 'e) t -> ('b, 'd, 'f) t *)
(*       val mk_pretty: *)
(*         (Format.formatter -> 'a -> unit) -> *)
(*         (Format.formatter -> 'b -> unit) -> *)
(*         (Format.formatter -> 'c -> unit) -> *)
(*         Format.formatter -> ('a, 'b, 'c) t -> unit *)
(*       val mk_mem_project: *)
(*         ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*         ((Project_skeleton.t -> bool) -> 'b -> bool) -> *)
(*         ((Project_skeleton.t -> bool) -> 'c -> bool) -> *)
(*         (Project_skeleton.t -> bool) -> ('a, 'b, 'c) t -> bool *)
(*     end) = *)
(*struct *)

(*  include Type.Polymorphic3(P) *)

(*  module Make(T1: S)(T2: S)(T3: S) = struct *)

(*    module T = struct *)
(*      type t = (T1.t, T2.t, T3.t) P.t *)
(*      let ty, _is_new = instantiate T1.ty T2.ty T3.ty *)
(*    end *)

(*    include T *)
(*    include Build *)
(*        (struct *)
(*          include T *)
(*          let reprs = Type.reprs ty *)
(*          let build mk f1 f2 f3 = *)
(*            if mk == undefined || f1 == undefined || f2 == undefined || *)
(*               f3 == undefined *)
(*            then *)
(*              undefined *)
(*            else *)
(*              mk f1 f2 f3 *)
(*          let compare = build P.mk_compare T1.compare T2.compare T3.compare *)
(*          let equal = build P.mk_equal T1.equal T2.equal T3.equal *)
(*          let hash = build P.mk_hash T1.hash T2.hash T3.hash *)
(*          let rehash = identity *)
(*          let copy = *)
(*            let mk f1 f2 f3 = *)
(*              if P.map == undefined then undefined *)
(*              else *)
(*                (1* [JS 2011/05/31] No optimisation for the special case of identity, *)
(*                   since we really want to perform a DEEP copy. *1) *)
(*                (1*if f1 == identity && f2 == identity then identity *)
(*                  else*) (* P.map f1 f2 f3 *)
(*            in *)
(*            build mk T1.copy T2.copy T3.copy *)
(*          let pretty = build P.mk_pretty T1.pretty T2.pretty T3.pretty *)
(*          let mem_project = *)
(*            let mk f1 f2 f3 = *)
(*              if P.mk_mem_project == undefined then undefined *)
(*              else if f1 == never_any_project && f2 == never_any_project *)
(*                      && f3 == never_any_project *)
(*              then *)
(*                never_any_project *)
(*              else *)
(*                P.mk_mem_project f1 f2 f3 *)
(*            in *)
(*            build mk T1.mem_project T2.mem_project T3.mem_project *)
(*        end) *)

(*    let datatype_descr, packed_descr = *)
(*      mk_full_descr *)
(*        (Descr.of_structural *)
(*           ty *)
(*           (P.structural_descr *)
(*              (Descr.str T1.datatype_descr) *)
(*              (Descr.str T2.datatype_descr) *)
(*              (Descr.str T3.datatype_descr))) *)

(*  end *)

(*end *)

(* ****************************************************************************)
(** {2 Polymorphic4 } *)
(* ****************************************************************************)

(*module type Polymorphic4 = sig *)
(*  include Type.Polymorphic4 *)
(*  module Make(T1:S)(T2:S)(T3:S)(T4:S) *)
(*    : S with type t = (T1.t, T2.t, T3.t, T4.t) poly *)
(*end *)

(*module Polymorphic4 *)
(*    (P: sig *)
(*       include Type.Polymorphic4_input *)
(*       val mk_equal: *)
(*         ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> *)
(*         ('c -> 'c -> bool) -> ('d -> 'd -> bool) -> *)
(*         ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> *)
(*         bool *)
(*       val mk_compare: *)
(*         ('a -> 'a -> int) -> ('b -> 'b -> int) -> *)
(*         ('c -> 'c -> int) -> ('d -> 'd -> int) -> *)
(*         ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t -> int *)
(*       val mk_hash: *)
(*         ('a -> int) -> ('b -> int) -> ('c -> int) -> ('d -> int) -> *)
(*         ('a, 'b, 'c, 'd) t -> int *)
(*       val map: *)
(*         ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> *)
(*         ('a, 'c, 'e, 'g) t -> ('b, 'd, 'f, 'h) t *)
(*       val mk_pretty: *)
(*         (Format.formatter -> 'a -> unit) -> *)
(*         (Format.formatter -> 'b -> unit) -> *)
(*         (Format.formatter -> 'c -> unit) -> *)
(*         (Format.formatter -> 'd -> unit) -> *)
(*         Format.formatter -> ('a, 'b, 'c, 'd) t -> unit *)
(*       val mk_mem_project: *)
(*         ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
(*         ((Project_skeleton.t -> bool) -> 'b -> bool) -> *)
(*         ((Project_skeleton.t -> bool) -> 'c -> bool) -> *)
(*         ((Project_skeleton.t -> bool) -> 'd -> bool) -> *)
(*         (Project_skeleton.t -> bool) -> ('a, 'b, 'c, 'd) t -> bool *)
(*     end) = *)
(*struct *)

(*  include Type.Polymorphic4(P) *)

(*  module Make(T1: S)(T2: S)(T3: S)(T4: S) = struct *)

(*    module T = struct *)
(*      type t = (T1.t, T2.t, T3.t, T4.t) P.t *)
(*      let ty, _is_new = instantiate T1.ty T2.ty T3.ty T4.ty *)
(*    end *)

(*    include T *)
(*    include Build *)
(*        (struct *)
(*          include T *)
(*          let reprs = Type.reprs ty *)
(*          let build mk f1 f2 f3 f4 = *)
(*            if mk == undefined || f1 == undefined || f2 == undefined || *)
(*               f3 == undefined || f4 == undefined *)
(*            then *)
(*              undefined *)
(*            else *)
(*              mk f1 f2 f3 f4 *)
(*          let compare = *)
(*            build P.mk_compare T1.compare T2.compare T3.compare T4.compare *)
(*          let equal = build P.mk_equal T1.equal T2.equal T3.equal T4.equal *)
(*          let hash = build P.mk_hash T1.hash T2.hash T3.hash T4.hash *)
(*          let rehash = identity *)
(*          let copy = *)
(*            let mk f1 f2 f3 f4 = *)
(*              if P.map == undefined then undefined *)
(*              else *)
(*                (1* [JS 2011/05/31] No optimisation for the special case of identity, *)
(*                   since we really want to perform a DEEP copy. *1) *)
(*                (1*if f1 == identity && f2 == identity then identity *)
(*                  else*) (* P.map f1 f2 f3 f4 *)
(*            in *)
(*            build mk T1.copy T2.copy T3.copy T4.copy *)
(*          let pretty = build P.mk_pretty T1.pretty T2.pretty T3.pretty T4.pretty *)
(*          let mem_project = *)
(*            let mk f1 f2 f3 f4 = *)
(*              if P.mk_mem_project == undefined then undefined *)
(*              else if f1 == never_any_project && f2 == never_any_project *)
(*                      && f3 == never_any_project && f4 == never_any_project *)
(*              then *)
(*                never_any_project *)
(*              else *)
(*                P.mk_mem_project f1 f2 f3 f4 *)
(*            in *)
(*            build mk T1.mem_project T2.mem_project T3.mem_project T4.mem_project *)
(*        end) *)

(*    let datatype_descr, packed_descr = *)
(*      mk_full_descr *)
(*        (Descr.of_structural *)
(*           ty *)
(*           (P.structural_descr *)
(*              (Descr.str T1.datatype_descr) *)
(*              (Descr.str T2.datatype_descr) *)
(*              (Descr.str T3.datatype_descr) *)
(*              (Descr.str T4.datatype_descr))) *)

(*  end *)

(*end *)

(* ****************************************************************************)
(** {3 Pair} *)
(* ****************************************************************************)

(* module Pair_arg = struct *)
(*   type ('a, 'b) t = 'a * 'b *)
(*   let reprs a b = [ a, b ] *)
(*   let structural_descr d1 d2 = *)
(*     Structural_descr.t_tuple *)
(*       [| Structural_descr.pack d1; Structural_descr.pack d2 |] *)
(*   let mk_equal f1 f2 (x1,x2) (y1,y2) = f1 x1 y1 && f2 x2 y2 *)
(*   let mk_compare f1 f2 (x1,x2 as x) (y1,y2 as y) = *)
(*     if x == y then 0 else let n = f1 x1 y1 in if n = 0 then f2 x2 y2 else n *)
(*   let mk_hash f1 f2 (x1,x2) = f1 x1 + 1351 * f2 x2 *)
(*   let map f1 f2 (x1,x2) = f1 x1, f2 x2 *)
(*   let mk_pretty f1 f2 fmt (x1, x2) = *)
(*     Format.fprintf fmt "(@[<hv 2>%a,@;%a@])" f1 x1 f2 x2 *)
(*   let mk_mem_project mem1 mem2 f (x1, x2) = mem1 f x1 && mem2 f x2 *)
(* end *)

(* warning is unsound in that case:
   http://caml.inria.fr/mantis/view.php?id=7314#c16232
*)
[@@@ warning "-60"]

(* module rec Pair_name: sig val name: 'a Type.t -> 'b Type.t -> string end = *)
(* struct *)
(*   let name ty1 ty2 = *)
(*     let arg ty = *)
(*       Type.par_ty_name *)
(*         (fun ty -> *)
(*            Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty) *)
(*         ty *)
(*     in *)
(*     arg ty1 ^ " * " ^ arg ty2 *)
(* end *)

(* and Poly_pair : sig *)
(*   include Type.Polymorphic2 with type ('a,'b) poly = 'a * 'b *)
(*   module Make(T1: S)(T2: S) : S with type t = (T1.t, T2.t) poly *)
(* end = *)
(* struct *)
(*   (1* Split the functor argument in 2 modules such that OCaml is able to safely *)
(*      evaluate the recursive modules *1) *)
(*   include Polymorphic2(struct include Pair_arg include Pair_name end) *)
(* end *)

(* [@@@ warning "+60"] *)

(* module Pair = Poly_pair.Make *)

(* let pair (type typ1) (type typ2) (ty1: typ1 Type.t) (ty2: typ2 Type.t) = *)
(*   let module Make(X: sig type t val ty: t Type.t end) = struct *)
(*     type t = X.t *)
(*     let ty = X.ty *)
(*     let datatype_name = Type.name X.ty *)
(*     let datatype_descr = Descr.of_type X.ty *)
(*     let packed_descr = Descr.pack datatype_descr *)
(*     let reprs = Type.reprs X.ty *)
(*     let equal = equal X.ty *)
(*     let compare = compare X.ty *)
(*     let hash = hash X.ty *)
(*     let copy = copy X.ty *)
(*     let pretty = pretty X.ty *)
(*     let mem_project = mem_project X.ty *)
(*   end *)
(*   in *)
(*   let module L = Pair *)
(*       (Make(struct type t = typ1 let ty = ty1 end)) *)
(*       (Make(struct type t = typ2 let ty = ty2 end)) *)
(*   in *)
(*   L.ty *)

(* ****************************************************************************)
(** {3 Function} *)
(* ****************************************************************************)

(* module Function *)
(*     (T1: sig include Ty val label: (string * (unit -> t) option) option end) *)
(*     (T2: Ty) = *)
(* struct *)
(*   module T = struct *)
(*     type t = T1.t -> T2.t *)
(*     let ty, _is_new = Type.Function.instantiate ?label:T1.label T1.ty T2.ty *)
(*     let compare = undefined *)
(*     let equal = (==) *)
(*     let hash = undefined *)
(*     let rehash = undefined *)
(*     let copy = undefined *)
(*     let pretty = undefined *)
(*     let mem_project = never_any_project *)
(*     let reprs = Type.reprs ty *)
(*   end *)
(*   include T *)
(*   include Build(T) *)
(* end *)

(* let func  (type typ1) (type typ2) ?label (ty1: typ1 Type.t) (ty2: typ2 Type.t) = *)
(*   let module L = Function *)
(*       (struct type t = typ1 let ty = ty1 let label = label end) *)
(*       (struct type t = typ2 let ty = ty2 end) *)
(*   in *)
(*   L.ty *)

(* let optlabel_func lab dft = func ~label:(lab, Some dft) *)

(* let func2 ?label1 ty1 ?label2 ty2 ty_ret = *)
(*   func ?label:label1 ty1 (func ?label:label2 ty2 ty_ret) *)

(* let func3 ?label1 ty1 ?label2 ty2 ?label3 ty3 ty_ret = *)
(*   func2 ?label1 ty1 ?label2 ty2 (func ?label:label3 ty3 ty_ret) *)

(* let func4 ?label1 ty1 ?label2 ty2 ?label3 ty3 ?label4 ty4 ty_ret = *)
(*   func3 ?label1 ty1 ?label2 ty2 ?label3 ty3 (func ?label:label4 ty4 ty_ret) *)

(* let is_function_or_pair ty = *)
(*   Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty *)

(* ****************************************************************************)
(** {2 Polymorphic generator} *)
(* ****************************************************************************)

module type Polymorphic_input = sig
  include Type.Polymorphic_input
  val mk_equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val mk_compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val mk_hash: ('a -> int) -> 'a t -> int
  val map: ('a -> 'b) -> 'a t -> 'b t
  (* val mk_pretty: *)
  (*   (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit *)
  (* val mk_mem_project: *)
  (*   ((Project_skeleton.t -> bool) -> 'a -> bool) -> *)
  (*   (Project_skeleton.t -> bool) -> 'a t -> bool *)
end

module Polymorphic_gen(P: Polymorphic_input) = struct

 include Type.Polymorphic(P)

 module Make_gen(X: S)(R: sig val rehash: X.t poly -> X.t poly end) = struct

   module T = struct
     type t = X.t P.t
     (* let ty, _is_new = instantiate X.ty *)
   end

   include T
   include
     Build
       (struct
         include T
         let build mk f =
           if mk == undefined || f == undefined then undefined else mk f
         let compare = build P.mk_compare X.compare
         let equal =
           if P.mk_equal == from_compare then
             if compare == undefined then undefined else from_compare
           else build P.mk_equal X.equal
         let hash = build P.mk_hash X.hash
         let copy =
           (* issue #36: do not use [build] here in order to be able to
              copy an empty datastructure even if the underlying function is
              undefined. The potential issue would be to not have the invariant
              that [copy] is [undefined] as soon as the underlying [copy] is;
              but the kernel does not rely on this behavior for that particular
              function (and hopefully it will not change in the future). *)
           if P.map == undefined then undefined
           else
             (* [JS 2011/05/31] No optimisation for the special case of
                identity, since we really want to perform a DEEP copy. *)
            (* if f == identity then identity else *)
             fun x -> P.map X.copy x
         let rehash = R.rehash
         (* let pretty = build P.mk_pretty X.pretty *)
         (* let mem_project = *)
         (*   let mk f = *)
         (*     if P.mk_mem_project == undefined then undefined *)
         (*     else if f == never_any_project then never_any_project *)
         (*     else fun p x -> P.mk_mem_project f p x *)
         (*   in *)
         (*   build mk X.mem_project *)
         (* let reprs = Type.reprs ty *)
       end)

   (* let datatype_descr, packed_descr = *)
   (*   mk_full_descr *)
   (*     (Descr.of_structural ty *)
   (*        (P.structural_descr (Descr.str X.datatype_descr))) *)

 end

end

(*module Polymorphic(P: Polymorphic_input) = struct *)
(*  include Polymorphic_gen(P) *)
(*  module Make(X: S) = *)
(*    Make_gen *)
(*      (X) *)
(*      (struct *)
(*        let rehash = *)
(*          if Descr.is_unmarshable X.datatype_descr then undefined *)
(*          else identity *)
(*      end) *)
(*end *)

(* ****************************************************************************)
(** {3 Reference} *)
(* ****************************************************************************)

(* module Poly_ref = *)
(*   Polymorphic *)
(*     (struct *)
(*       type 'a t = 'a ref *)
(*       let name ty = Type.par_ty_name is_function_or_pair ty ^ " ref" *)
(*       let reprs ty = [ ref ty ] *)
(*       let structural_descr = Structural_descr.t_ref *)
(*       let mk_equal f x y = f !x !y *)
(*       let mk_compare f x y = if x == y then 0 else f !x !y *)
(*       let mk_hash f x = f !x *)
(*       let map f x = ref (f !x) *)
(*       let mk_pretty f fmt x = *)
(*         Format.fprintf fmt "(@[<hv 2>ref@;%a@])" f !x *)
(*       let mk_mem_project mem f x = mem f !x *)
(*     end) *)

(* module Ref = Poly_ref.Make *)

(* let t_ref (type typ) (ty: typ Type.t) = *)
(*   let module L = *)
(*     Ref(struct *)
(*       type t = typ *)
(*       let ty = ty *)
(*       let datatype_name = Type.name ty *)
(*       let datatype_descr = Descr.of_type ty *)
(*       let packed_descr = Descr.pack datatype_descr *)
(*       let reprs = Type.reprs ty *)
(*       let equal = equal ty *)
(*       let compare = compare ty *)
(*       let hash = hash ty *)
(*       let copy = copy ty *)
(*       let pretty = pretty ty *)
(*       let mem_project = mem_project ty *)
(*     end) *)
(*   in *)
(*   L.ty *)

(* ****************************************************************************)
(** {3 Option} *)
(* ****************************************************************************)

(* module Poly_option = *)
(*   Polymorphic *)
(*     (struct *)
(*       type 'a t = 'a option *)
(*       let name ty = Type.par_ty_name is_function_or_pair ty ^ " option" *)
(*       let reprs ty = [ Some ty ] *)
(*       let structural_descr = Structural_descr.t_option *)
(*       let mk_equal f x y = match x, y with *)
(*         | None, None -> true *)
(*         | None, Some _ | Some _, None -> false *)
(*         | Some x, Some y -> f x y *)
(*       let mk_compare f x y = *)
(*         if x == y then 0 *)
(*         else match x, y with *)
(*           | None, None -> 0 *)
(*           | None, Some _ -> 1 *)
(*           | Some _, None -> -1 *)
(*           | Some x, Some y -> f x y *)
(*       let mk_hash f = function None -> 0 | Some x -> f x *)
(*       let map f = function None -> None | Some x -> Some (f x) *)
(*       let mk_pretty f fmt = function *)
(*         | None -> Format.fprintf fmt "None" *)
(*         | Some x -> *)
(*           Format.fprintf fmt "(@[<hv 2>Some@;%a@])" f x *)
(*       let mk_mem_project mem f = function None -> false | Some x -> mem f x *)
(*     end) *)

(* module Option = Poly_option.Make *)


(* let option (type typ) (ty: typ Type.t) = *)
(*   let module L = *)
(*     Option(struct *)
(*       type t = typ *)
(*       let ty = ty *)
(*       let datatype_name = Type.name ty *)
(*       let datatype_descr = Descr.of_type ty *)
(*       let packed_descr = Descr.pack datatype_descr *)
(*       let reprs = Type.reprs ty *)
(*       let equal = equal ty *)
(*       let compare = compare ty *)
(*       let hash = hash ty *)
(*       let copy = copy ty *)
(*       let pretty = pretty ty *)
(*       let mem_project = mem_project ty *)
(*     end) *)
(*   in *)
(*   L.ty *)

(* ****************************************************************************)
(** {3 List} *)
(* ****************************************************************************)

(* module Poly_list = *)
(*   Polymorphic *)
(*     (struct *)
(*       type 'a t = 'a list *)
(*       let name ty = Type.par_ty_name is_function_or_pair ty ^ " list" *)
(*       let reprs ty = [ [ ty ] ] *)
(*       let structural_descr = Structural_descr.t_list *)
(*       let mk_equal f l1 l2 = *)
(*         try List.for_all2 f l1 l2 with Invalid_argument _ -> false *)
(*       let rec mk_compare f l1 l2 = *)
(*         if l1 == l2 then 0 *)
(*         else match l1, l2 with *)
(*           | [], [] -> assert false *)
(*           | [], _ :: _ -> -1 *)
(*           | _ :: _, [] -> 1 *)
(*           | x1 :: q1, x2 :: q2 -> *)
(*             let n = f x1 x2 in *)
(*             if n = 0 then mk_compare f q1 q2 else n *)
(*       exception Too_long of int *)
(*       (1* Do not spend too much time hashing long lists... *1) *)
(*       let mk_hash f l = *)
(*         try *)
(*           snd (List.fold_left *)
(*                  (fun (length,acc) d -> *)
(*                     if length > 15 then raise (Too_long acc); *)
(*                     length+1, 257 * acc + f d) *)
(*                  (0,1) l) *)
(*         with Too_long n -> n *)
(*       let map = List.map *)
(*       let mk_pretty f fmt l = *)
(*         Format.fprintf fmt "(@[<hv 2>[ %t ]@])" *)
(*           (fun fmt -> *)
(*              let rec print fmt = function *)
(*                | [] -> () *)
(*                | [ x ] -> Format.fprintf fmt "%a" f x *)
(*                | x :: l -> Format.fprintf fmt "%a;@;%a" f x print l *)
(*              in *)
(*              print fmt l) *)
(*       let mk_mem_project mem f = List.exists (mem f) *)
(*     end) *)

(* module Caml_list = List *)
(* module List = Poly_list.Make *)

(* let list (type typ) (ty: typ Type.t) = *)
(*   let module L = *)
(*     List(struct *)
(*       type t = typ *)
(*       let ty = ty *)
(*       let datatype_name = Type.name ty *)
(*       let datatype_descr = Descr.of_type ty *)
(*       let packed_descr = Descr.pack datatype_descr *)
(*       let reprs = Type.reprs ty *)
(*       let equal = equal ty *)
(*       let compare = compare ty *)
(*       let hash = hash ty *)
(*       let copy = copy ty *)
(*       let pretty = pretty ty *)
(*       let mem_project = mem_project ty *)
(*     end) *)
(*   in *)
(*   L.ty *)

(* ****************************************************************************)
(** {3 Arrays} *)
(* ****************************************************************************)

(* module Poly_array = *)
(*   Polymorphic *)
(*     (struct *)
(*       type 'a t = 'a array *)
(*       let name ty = Type.par_ty_name is_function_or_pair ty ^ " array" *)
(*       let reprs ty = [ [| ty |] ] *)
(*       let structural_descr = Structural_descr.t_array *)
(*       exception Early_exit of int *)
(*       let mk_equal f a1 a2 = *)
(*         let size = Array.length a1 in *)
(*         if Array.length a2 != size then false *)
(*         else try *)
(*             for i = 0 to size - 1 do *)
(*               if not (f a1.(i) a2.(i)) then raise (Early_exit 0) *)
(*             done; *)
(*             true *)
(*           with Early_exit _ -> false *)
(*       ;; *)
(*       let mk_compare f a1 a2 = *)
(*         if a1 == a2 then 0 *)
(*         else let size1 = Array.length a1 and size2 = Array.length a2 in *)
(*           if size1 < size2 then -1 *)
(*           else if size2 > size1 then 1 *)
(*           else try *)
(*               for i = 0 to size1 do *)
(*                 let n = f a1.(i) a2.(i) in *)
(*                 if n != 0 then raise (Early_exit n) *)
(*               done; *)
(*               0 *)
(*             with Early_exit n -> n *)
(*       ;; *)
(*       (1* Do not spend too much time hashing long arrays... *1) *)
(*       let mk_hash f a = *)
(*         let max = max 15 ((Array.length a) - 1) in *)
(*         let acc = ref 1 in *)
(*         for i = 0 to max do acc := 257 * !acc + f a.(i) done; *)
(*         !acc *)
(*       ;; *)
(*       let map = Array.map *)
(*       let mk_pretty f = *)
(*         if f == undefined *)
(*         then undefined *)
(*         else fun fmt a -> *)
(*           Format.fprintf fmt "(@[<hv 2>[| %t |]@])" *)
(*             (fun fmt -> *)
(*                let length = Array.length a in *)
(*                match length with *)
(*                | 0 -> () *)
(*                | _ -> (Format.fprintf fmt "%a" f a.(0); *)
(*                        for i = 1 to (length - 1) do *)
(*                          Format.fprintf fmt ";@;%a" f a.(i) *)
(*                        done)) *)
(*       let mk_mem_project mem f a = *)
(*         try *)
(*           for i = 0 to (Array.length a - 1) do *)
(*             if mem f a.(i) then raise (Early_exit 0) *)
(*           done; *)
(*           false *)
(*         with Early_exit _ -> true *)
(*     end) *)

(* module Array = Poly_array.Make *)

(* let array (type typ) (ty: typ Type.t) = *)
(*   let module L = *)
(*     Array(struct *)
(*       type t = typ *)
(*       let ty = ty *)
(*       let datatype_name = Type.name ty *)
(*       let datatype_descr = Descr.of_type ty *)
(*       let packed_descr = Descr.pack datatype_descr *)
(*       let reprs = Type.reprs ty *)
(*       let equal = equal ty *)
(*       let compare = compare ty *)
(*       let hash = hash ty *)
(*       let copy = copy ty *)
(*       let pretty = pretty ty *)
(*       let mem_project = mem_project ty *)
(*     end) *)
(*   in *)
(*   L.ty *)


(* ****************************************************************************)
(** {3 Queue} *)
(* ****************************************************************************)

(* module Poly_queue = *)
(*   Polymorphic *)
(*     (struct *)
(*       type 'a t = 'a Queue.t *)
(*       let name ty = Type.par_ty_name is_function_or_pair ty ^ " Queue.t" *)
(*       let reprs x = *)
(*         let q = Queue.create () in *)
(*         Queue.add x q; *)
(*         [ q ] *)
(*       let structural_descr = Structural_descr.t_queue *)
(*       let mk_equal = undefined *)
(*       let mk_compare = undefined *)
(*       let mk_hash = undefined *)
(*       let map = undefined *)
(*       let mk_pretty = undefined *)
(*       let mk_mem_project mem f q = *)
(*         try Queue.iter (fun x -> if mem f x then raise Exit) q; false *)
(*         with Exit -> true *)
(*     end) *)

(* module Queue = Poly_queue.Make *)

(* let queue (type typ) (ty: typ Type.t) = *)
(*   let module L = *)
(*     Queue(struct *)
(*       type t = typ *)
(*       let ty = ty *)
(*       let datatype_name = Type.name ty *)
(*       let datatype_descr = Descr.of_type ty *)
(*       let packed_descr = Descr.pack datatype_descr *)
(*       let reprs = Type.reprs ty *)
(*       let equal = equal ty *)
(*       let compare = compare ty *)
(*       let hash = hash ty *)
(*       let copy = copy ty *)
(*       let pretty = pretty ty *)
(*       let mem_project = mem_project ty *)
(*     end) *)
(*   in *)
(*   L.ty *)

(* ****************************************************************************)
(** {3 Set} *)
(* ****************************************************************************)

(* (1* OCaml functors are generative *1) *)
module Set(S: Set.S)(E: S with type t = S.elt) =
struct

  let () = check E.equal "equal" E.datatype_name
  let () = check E.compare "compare" E.datatype_name

  module P = Make
      (struct
        type t = S.t
        let name = "Set(" ^ E.datatype_name ^ ")"
        (* let structural_descr = *)
        (*   Structural_descr.t_set_unchanged_compares (Descr.str E.datatype_descr) *)
        open S
        (* let reprs = empty :: Caml_list.map (fun r -> singleton r) E.reprs *)
        let compare = S.compare
        let equal = S.equal
        let hash =
          if E.hash == undefined then undefined
          else (fun s -> S.fold (fun e h -> 67 * E.hash e + h) s 189)
        let rehash =
          (* if Descr.is_unmarshable E.datatype_descr then undefined *)
          (* else if Descr.is_abstract E.datatype_descr then identity *)
          (* else *)
            fun s -> (* The key changed, rebalance the tree *)
              S.fold S.add s S.empty
        let copy =
          (* [JS 2011/05/31] No optimisation for the special case of
             identity, since we really want to perform a DEEP copy. *)
          (*      if E.copy == identity then identity
                  else*) 
          fun s -> S.fold (fun x -> S.add (E.copy x)) s S.empty

        (* let pretty = *)
        (*   if E.pretty == undefined *)
        (*   then undefined *)
        (*   else fun fmt s -> *)
        (*     let pp_elt pp fmt v = *)
        (*       Format.fprintf fmt "@[%a@]" pp v *)
        (*     in *)
        (*     Pretty_utils.pp_iter *)
        (*       ~pre:"@[<hov 2>{@ " ~sep:";@ " ~suf:"@ }@]" *)
        (*       S.iter (pp_elt E.pretty) fmt s *)

        (* let mem_project p s = *)
        (*   try S.iter (fun x -> if E.mem_project p x then raise Exit) s; false *)
        (*   with Exit -> true *)
      end)
  include S

  let nearest_elt_le x = S.find_last (fun y -> y <= x)
  let nearest_elt_ge x = S.find_first (fun y -> y >= x)

  (* let ty = P.ty *)
  let datatype_name = P.datatype_name
  (* let datatype_descr = P.datatype_descr *)
  (* let packed_descr = P.packed_descr *)
  (* let reprs = P.reprs *)
  let equal = P.equal
  let compare = P.compare
  let hash = P.hash
  (* let pretty = P.pretty *)
  (* let mem_project = P.mem_project *)
  let copy = P.copy

end

(* ****************************************************************************)
(** {3 Map} *)
(* ****************************************************************************)

module Map(M: Map.S)(Key: S with type t = M.key) =
struct

  let () = check Key.equal "equal" Key.datatype_name
  let () = check Key.compare "compare" Key.datatype_name

  module P_gen = Polymorphic_gen
      (struct
        type 'a t = 'a M.t
        let name ty = "Map(" ^ Key.datatype_name ^ ", " ^ Type.name ty ^ ")"
        (* let structural_descr d = *)
        (*   Structural_descr.t_map_unchanged_compares *)
        (*     (Descr.str Key.datatype_descr) d *)
        open M
        (* let reprs r = *)
        (*   [ Caml_list.fold_left (fun m k -> add k r m) empty Key.reprs ] *)
        let mk_compare = M.compare
        let mk_equal = M.equal
        let mk_hash = undefined
        let map = M.map
        (* let mk_pretty f_value = *)
        (*   if Key.pretty == undefined || f_value == undefined *)
        (*   then undefined *)
        (*   else fun fmt map -> *)
        (*     Format.fprintf fmt  "@[{{ "; *)
        (*     M.iter *)
        (*       (fun k v -> *)
        (*          Format.fprintf fmt "@[@[%a@] -> @[%a@]@];@ " *)
        (*            Key.pretty k *)
        (*            f_value v) *)
        (*       map; *)
        (*     Format.fprintf fmt  " }}@]" *)
        (* let mk_mem_project = *)
        (*   if Key.mem_project == undefined then undefined *)
        (*   else *)
        (*     fun mem -> *)
        (*       if mem == never_any_project && Key.mem_project == never_any_project *)
        (*       then never_any_project *)
        (*       else *)
        (*         fun p m -> *)
        (*           try *)
        (*             M.iter *)
        (*               (fun k v -> *)
        (*                  if Key.mem_project p k || mem p v then raise Exit) *)
        (*               m; *)
        (*             false *)
        (*           with Exit -> *)
        (*             true *)
      end)

  module P = struct
    include P_gen
    module Make(X:S) =
      Make_gen
        (X)
        (struct
          let rehash =
            (* if Descr.is_unmarshable Key.datatype_descr *)
            (* || Descr.is_unmarshable X.datatype_descr *)
            (* then undefined *)
            (* else *)
            (* if Descr.is_abstract Key.datatype_descr then identity *)
            (* else (1* the key changed: rebuild the map *1) *)
              fun m ->
                M.fold M.add m M.empty;
        end)
  end


  include M
  module Key = Key
  module Make = P.Make
end

(* ****************************************************************************)
(** {3 Hashtbl} *)
(* ****************************************************************************)

(* (1* OCaml functors are generative *1) *)
module Hashtbl(H: Hashtbl_with_descr)(Key: S with type t = H.key) =
struct

  let () = check Key.equal "equal" Key.datatype_name
  let () = check Key.hash "hash" Key.datatype_name

  module P_gen = Polymorphic_gen
      (struct
        type 'a t = 'a H.t
        let name ty = "Hashtbl(" ^ Key.datatype_name ^ ", " ^ Type.name ty ^ ")"
        (* let structural_descr = H.structural_descr *)
        (* let reprs x = *)
        (*   [ let h = H.create 7 in *)
        (*     Caml_list.iter (fun k -> H.add h k x) Key.reprs; h ] *)
        let mk_compare = undefined
        let mk_equal = from_compare
        let mk_hash = undefined
        let map f_value tbl =
          (* first mapping which reverses the binding order *)
          let h = H.create (H.length tbl) (* may be very memory-consuming *) in
          H.iter (fun k v -> H.add h k (f_value v)) tbl;
          (* copy which reverses again the binding order: so we get the right
             order *)
          let h2 = H.create (H.length tbl) (* may be very memory-consuming *) in
          H.iter (fun k v -> H.add h2 k v) h;
          h2
        (* let mk_pretty = undefined *)
        (* let mk_mem_project = *)
        (*   if Key.mem_project == undefined then undefined *)
        (*   else *)
        (*     fun mem -> *)
        (*       if mem == never_any_project && Key.mem_project == never_any_project *)
        (*       then never_any_project *)
        (*       else *)
        (*         fun p m -> *)
        (*           try *)
        (*             H.iter *)
        (*               (fun k v -> *)
        (*                  if Key.mem_project p k || mem p v then raise Exit) *)
        (*               m; *)
        (*             false *)
        (*           with Exit -> *)
        (*             true *)
      end)

  module P = struct
    include P_gen
    module Make(X:S) =
      Make_gen
        (X)
        (struct
          let rehash =
            (* if Descr.is_unmarshable Key.datatype_descr *)
            (* || Descr.is_unmarshable X.datatype_descr *)
            (* then undefined *)
            (* else *)
            (* if Descr.is_abstract Key.datatype_descr then identity *)
            (* else (1* the key changed: rebuild the hashtbl *1) *)
              fun h ->
                let h' = H.create (H.length h) in
                H.iter (H.add h') h;
                h'
        end)

  end

  include H

  (* let make_type (type typ) (ty: typ Type.t) = *)
  (*   let module M = *)
  (*     P.Make(struct *)
  (*       type t = typ *)
  (*       include Undefined *)
  (*       let ty = ty *)
  (*       let datatype_name = Type.name ty *)
  (*       let datatype_descr = Descr.of_type ty *)
  (*       let packed_descr = Descr.pack datatype_descr *)
  (*       let reprs = Type.reprs ty *)
  (*     end) *)
  (*   in M.ty *)

  module Key = Key
  module Make = P.Make

end

(* ****************************************************************************)
(** {3 Weak hashtbl} *)
(* ****************************************************************************)

module type Sub_caml_weak_hashtbl = sig
  type data
  type t
  val create: int -> t
  val add: t -> data -> unit
end

(* module Initial_caml_weak = Weak *)

(* module Weak(W: Sub_caml_weak_hashtbl)(D: S with type t = W.data) = struct *)
(*   include Make *)
(*       (struct *)
(*         include Undefined *)
(*         type t = W.t *)
(*         let name = "Weak(" ^ D.datatype_name ^ ")" *)
(*         let reprs = let w = W.create 0 in Caml_list.iter (W.add w) D.reprs; [ w ] *)
(*       end) *)
(* end *)

(* module Caml_weak_hashtbl(D: S) = struct *)
(*   let () = check D.equal "equal" D.datatype_name *)
(*   let () = check D.compare "hash" D.datatype_name *)
(*   module W = Initial_caml_weak.Make(D) *)
(*   include W *)
(*   module Datatype = Weak(W)(D) *)
(* end *)

(* ****************************************************************************)
(** {2 Simple type values} *)
(* ****************************************************************************)

module With_set_and_map(X: S) = struct

  module D = X
  include D

  module Set = Set (Stdlib.Set.Make(D))(D)
  module Map = Map (Stdlib.Map.Make(D))(D)

end

module Make_with_set_and_map(X: Make_input) =
  With_set_and_map (Make(X))

module With_hashtbl(X: S) = struct

  module D = X
  include D

  module Hashtbl =
    Hashtbl
      (struct
        include Fc_hashtbl.Make(D)

        (* Override "sorted" iterators by using the datatype comparison
           function if it has been supplied *)
        (* let iter_sorted ?cmp = match cmp with *)
        (*   | None -> *)
        (*     if D.compare == undefined then iter_sorted ?cmp:None *)
        (*     else iter_sorted ~cmp:D.compare *)
        (*   | Some cmp -> iter_sorted ~cmp *)

        (* let fold_sorted ?cmp = match cmp with *)
        (*   | None -> *)
        (*     if D.compare == undefined then fold_sorted ?cmp:None *)
        (*     else fold_sorted ~cmp:D.compare *)
        (*   | Some cmp -> fold_sorted ~cmp *)

        (* let structural_descr = *)
        (*   Structural_descr.t_hashtbl_unchanged_hashes *)
        (*     (Descr.str D.datatype_descr) *)
      end)
      (D)

end

module Make_with_hashtbl(X: Make_input) = With_hashtbl(Make(X))

module With_collections(X: S) = struct
  include (With_set_and_map(X) : S_with_set_and_map with type t = X.t)
  include (With_hashtbl(X) : S_with_hashtbl with type t := X.t)
end

module Make_with_collections(X: Make_input) = With_collections (Make(X))

(* ****************************************************************************)
(** {2 Predefined datatype} *)
(* ****************************************************************************)

module Simple_type
    (X: sig
       type t
       val name: string
       val reprs: t list
       val pretty: Format.formatter -> t -> unit
       val copy: t -> t
       val compare: t -> t -> int
       val equal: t -> t -> bool
     end) =
struct

  include With_collections
      (Make(struct
         type t = X.t
         let name = X.name
         (* let reprs = X.reprs *)
         (* let structural_descr = Structural_descr.t_abstract *)
         let equal = X.equal
         let compare = X.compare
         let hash = Fc_hashtbl.hash
         let rehash = identity
         let copy = X.copy
         (* let pretty = X.pretty *)
         (* let mem_project = never_any_project *)
       end))

end

module Unit =
  Simple_type
    (struct
      type t = unit
      let name = "unit"
      let reprs = [ () ]
      let copy = identity
      let compare () () = 0
      let equal () () = true
      let pretty fmt () = Format.fprintf fmt "()"
    end)
(* let unit = Unit.ty *)

module Bool =
  Simple_type
    (struct
      type t = bool
      let name = "bool"
      let reprs = [ true ]
      let copy = identity
      let compare : bool -> bool -> int = Stdlib.compare
      let equal : bool -> bool -> bool = (=)
      let pretty fmt b = Format.fprintf fmt "%B" b
    end)
(* let bool = Bool.ty *)

module Int = struct
  include Simple_type
      (struct
        type t = int
        let name = "int"
        let reprs = [ 2 ]
        let copy = identity
        let compare : int -> int -> int = Stdlib.compare
        let equal : int -> int -> bool = (=)
        let pretty fmt n = Format.fprintf fmt "%d" n
      end)
  let compare : int -> int -> int = Stdlib.compare
end
(* let int = Int.ty *)

module Int32 =
  Simple_type
    (struct
      type t = int32
      let name = "int32"
      let reprs = [ Int32.zero ]
      let copy = identity
      let compare = Int32.compare
      let equal : int32 -> int32 -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%ld" n
    end)
(* let int32 = Int32.ty *)

module Int64 =
  Simple_type
    (struct
      type t = int64
      let name = "int64"
      let reprs = [ Int64.zero ]
      let copy = identity
      let compare = Int64.compare
      let equal : int64 -> int64 -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%Ld" n
    end)
(* let int64 = Int64.ty *)

module Nativeint =
  Simple_type
    (struct
      type t = nativeint
      let name = "nativeint"
      let reprs = [ Nativeint.zero ]
      let copy = identity
      let compare = Nativeint.compare
      let equal : nativeint -> nativeint -> bool = (=)
      let pretty fmt n = Format.fprintf fmt "%nd" n
    end)
(* let nativeint = Nativeint.ty *)

module Float =
  Simple_type
    (struct
      type t = float
      let name = "float"
      let reprs = [ 0.1 ]
      let copy = identity
      let compare : float -> float -> int = Stdlib.compare
      let equal : float -> float -> bool = (=)
      let pretty fmt f = Format.fprintf fmt "%f" f
    end)
(* let float = Float.ty *)

module Char =
  Simple_type
    (struct
      type t = char
      let name = "char"
      let reprs = [ ' ' ]
      let copy = identity
      let compare = Char.compare
      let equal : char -> char -> bool = (=)
      let pretty fmt c = Format.fprintf fmt "%c" c
    end)
(* let char = Char.ty *)

module String =
  Simple_type
    (struct
      type t = string
      let name = "string"
      let reprs = [ "" ]
      let copy = Fun.id (* immutable strings do not need copy. *)
      let compare = String.compare
      let equal : string -> string -> bool = (=)
      let pretty fmt s = Format.fprintf fmt "%S" s
    end)
(* let string = String.ty *)

module Formatter =
  Make
    (struct
      type t = Format.formatter
      let name = "Datatype.Formatter"
      let reprs = [ Format.std_formatter ]
      (* let structural_descr = Structural_descr.t_unknown *)
      let equal = undefined
      let compare = undefined
      let hash = undefined
      let rehash = undefined
      let copy = undefined
      let pretty = undefined
      let mem_project = never_any_project
    end)
(* let formatter = Formatter.ty *)

module Integer =
  Make_with_collections
    (struct
      type t = Integer.t
      let name = "Datatype.Integer"
      let reprs = [ Integer.zero ]
      (* let structural_descr = Structural_descr.t_abstract *)
      let equal = Integer.equal
      let compare = Integer.compare
      let hash = Integer.hash
      let rehash = identity
      let copy = identity
      (* TODO: this should take into account kernel's option -big-ints-hex *)
      let pretty = Integer.pretty
      let mem_project = never_any_project
    end)
(* let integer = Integer.ty *)

module Rational =
  Make_with_collections
    (struct
      type t = Q.t
      let name = "Datatype.Rational"
      let reprs = [ Q.zero ; Q.one ]
      (* let structural_descr = Structural_descr.t_abstract *)
      let equal = Q.equal
      let compare = Q.compare
      let copy = identity
      let rehash = identity
      let mem_project = never_any_project
      let pretty fmt q = Format.pp_print_float fmt (Q.to_float q)
      let hash q = Stdlib.Hashtbl.hash (Z.hash (Q.num q), Z.hash (Q.den q))
    end)
(* let rational = Rational.ty *)

(* ****************************************************************************)
(** {3 Triple} *)
(* ****************************************************************************)

module Triple_arg = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c
  let reprs a b c = [ a, b, c ]
  (* let structural_descr d1 d2 d3 = *)
  (*   Structural_descr.t_tuple *)
  (*     [| Structural_descr.pack d1; *)
  (*        Structural_descr.pack d2; *)
  (*        Structural_descr.pack d3 |] *)
  let mk_equal f1 f2 f3 (x1,x2,x3) (y1,y2,y3) = f1 x1 y1 && f2 x2 y2 && f3 x3 y3
  let mk_compare f1 f2 f3 (x1,x2,x3 as x) (y1,y2,y3 as y) =
    if x == y then 0
    else
      let n = f1 x1 y1 in
      if n = 0 then let n = f2 x2 y2 in if n = 0 then f3 x3 y3 else n
      else n
  let mk_hash f1 f2 f3 (x1,x2,x3) = f1 x1 + 1351 * f2 x2 + 257 * f3 x3
  let map f1 f2 f3 (x1,x2,x3) = f1 x1, f2 x2, f3 x3
  let mk_pretty f1 f2 f3 fmt (x1, x2, x3) =
    Format.fprintf fmt "(@[<hv 2>%a,@;%a,@;%a@])" f1 x1 f2 x2 f3 x3
  let mk_mem_project mem1 mem2 mem3 f (x1, x2, x3) =
    mem1 f x1 && mem2 f x2 && mem3 f x3
end

(* warning is unsound in that case:
    http://caml.inria.fr/mantis/view.php?id=7314#c16232
*)
[@@@ warning "-60"]

(* module rec Triple_name: sig *)
(*   val name: 'a Type.t -> 'b Type.t -> 'c Type.t -> string *)
(* end = *)
(* struct *)
(*   let name ty1 ty2 ty3 = *)
(*     let arg ty = *)
(*       Type.par_ty_name *)
(*         (fun ty -> *)
(*            Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty *)
(*            || Poly_triple.is_instance_of ty) *)
(*         ty *)
(*     in *)
(*     arg ty1 ^ " * " ^ arg ty2 ^ " * " ^ arg ty3 *)
(* end *)

(* and Poly_triple : sig *)
(*   include Type.Polymorphic3 with type ('a,'b,'c) poly = 'a * 'b * 'c *)
(*   module Make(T1: S)(T2: S)(T3:S) :  S with type t = (T1.t, T2.t, T3.t) poly *)
(* end = *)
(*   (1* Split the functor argument in 2 modules such that OCaml is able to safely *)
(*      evaluate the recursive modules *1) *)
(*   Polymorphic3(struct include Triple_arg include Triple_name end) *)

[@@@warning "+60"]

(* module Triple = Poly_triple.Make *)

(* let triple *)
(*     (type typ1) (type typ2) (type typ3) *)
(*     (ty1: typ1 Type.t) (ty2: typ2 Type.t) (ty3: typ3 Type.t) = *)
(*   let module Make(X: sig type t val ty: t Type.t end) = struct *)
(*     type t = X.t *)
(*     let ty = X.ty *)
(*     let datatype_name = Type.name X.ty *)
(*     let datatype_descr = Descr.of_type X.ty *)
(*     let packed_descr = Descr.pack datatype_descr *)
(*     let reprs = Type.reprs X.ty *)
(*     let equal = equal X.ty *)
(*     let compare = compare X.ty *)
(*     let hash = hash X.ty *)
(*     let copy = copy X.ty *)
(*     let pretty = pretty X.ty *)
(*     let mem_project = mem_project X.ty *)
(*   end *)
(*   in *)
(*   let module L = Triple *)
(*       (Make(struct type t = typ1 let ty = ty1 end)) *)
(*       (Make(struct type t = typ2 let ty = ty2 end)) *)
(*       (Make(struct type t = typ3 let ty = ty3 end)) *)
(*   in *)
(*   L.ty *)

(* ****************************************************************************)
(** {3 Quadruple} *)
(* ****************************************************************************)

(* module Quadruple_arg = struct *)
(*   type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd *)
(*   let reprs a b c d = [ a, b, c, d ] *)
(*   let structural_descr d1 d2 d3 d4 = *)
(*     Structural_descr.t_tuple *)
(*       [| Structural_descr.pack d1; *)
(*          Structural_descr.pack d2; *)
(*          Structural_descr.pack d3; *)
(*          Structural_descr.pack d4 |] *)
(*   let mk_equal f1 f2 f3 f4 (x1,x2,x3,x4) (y1,y2,y3,y4) = *)
(*     f1 x1 y1 && f2 x2 y2 && f3 x3 y3 && f4 x4 y4 *)
(*   let mk_compare f1 f2 f3 f4 (x1,x2,x3,x4 as x) (y1,y2,y3,y4 as y) = *)
(*     if x == y then 0 *)
(*     else *)
(*       let n = f1 x1 y1 in *)
(*       if n = 0 then *)
(*         let n = f2 x2 y2 in *)
(*         if n = 0 then let n = f3 x3 y3 in if n = 0 then f4 x4 y4 else n *)
(*         else n *)
(*       else n *)
(*   let mk_hash f1 f2 f3 f4 (x1,x2,x3,x4) = *)
(*     f1 x1 + 1351 * f2 x2 + 257 * f3 x3 + 997 * f4 x4 *)
(*   let map f1 f2 f3 f4 (x1,x2,x3,x4) = f1 x1, f2 x2, f3 x3, f4 x4 *)
(*   let mk_pretty f1 f2 f3 f4 fmt (x1, x2, x3, x4) = *)
(*     Format.fprintf fmt "(@[<hv 2>%a,@;%a,@;%a,@;%a@])" f1 x1 f2 x2 f3 x3 f4 x4 *)
(*   let mk_mem_project mem1 mem2 mem3 mem4 f (x1, x2, x3, x4) = *)
(*     mem1 f x1 && mem2 f x2 && mem3 f x3 && mem4 f x4 *)
(* end *)

(* warning is unsound in that case:
    http://caml.inria.fr/mantis/view.php?id=7314#c16232
*)
[@@@ warning "-60"]

(* module rec Quadruple_name: sig *)
(*   val name: 'a Type.t -> 'b Type.t -> 'c Type.t -> 'd Type.t -> string *)
(* end = *)
(* struct *)
(*   let name ty1 ty2 ty3 ty4 = *)
(*     let arg ty = *)
(*       Type.par_ty_name *)
(*         (fun ty -> *)
(*            Type.Function.is_instance_of ty || Poly_pair.is_instance_of ty *)
(*            || Poly_triple.is_instance_of ty || Poly_quadruple.is_instance_of ty) *)
(*         ty *)
(*     in *)
(*     arg ty1 ^ " * " ^ arg ty2 ^ " * " ^ arg ty3 ^ " * " ^ arg ty4 *)
(* end *)

(* and Poly_quadruple : sig *)
(*   include Type.Polymorphic4 with type ('a,'b,'c,'d) poly = 'a * 'b * 'c * 'd *)
(*   module Make(T1: S)(T2: S)(T3:S)(T4:S) : *)
(*     S with type t = (T1.t, T2.t, T3.t, T4.t) poly *)
(* end = *)
(* struct *)
(*   (1* Split the functor argument in 2 modules such that OCaml is able to safely *)
(*      evaluate the recursive modules *1) *)
(*   include Polymorphic4 *)
(*       (struct include Quadruple_arg include Quadruple_name end) *)
(* end *)

[@@@ warning "+60"]

(* module Quadruple = Poly_quadruple.Make *)

(* let quadruple *)
(*     (type typ1) (type typ2) (type typ3) (type typ4) *)
(*     (ty1: typ1 Type.t) (ty2: typ2 Type.t) (ty3: typ3 Type.t) (ty4: typ4 Type.t) *)
(*   = *)
(*   let module Make(X: sig type t val ty: t Type.t end) = struct *)
(*     type t = X.t *)
(*     let ty = X.ty *)
(*     let datatype_name = Type.name X.ty *)
(*     let datatype_descr = Descr.of_type X.ty *)
(*     let packed_descr = Descr.pack datatype_descr *)
(*     let reprs = Type.reprs X.ty *)
(*     let equal = equal X.ty *)
(*     let compare = compare X.ty *)
(*     let hash = hash X.ty *)
(*     let copy = copy X.ty *)
(*     let pretty = pretty X.ty *)
(*     let mem_project = mem_project X.ty *)
(*   end *)
(*   in *)
(*   let module L = Quadruple *)
(*       (Make(struct type t = typ1 let ty = ty1 end)) *)
(*       (Make(struct type t = typ2 let ty = ty2 end)) *)
(*       (Make(struct type t = typ3 let ty = ty3 end)) *)
(*       (Make(struct type t = typ4 let ty = ty4 end)) *)
(*   in *)
(*   L.ty *)

(* module Pair_with_collections(T1: S)(T2: S) = *)
(*   With_collections(Pair(T1)(T2)) *)

(* module Triple_with_collections(T1: S)(T2: S)(T3: S) = *)
(*   With_collections(Triple(T1)(T2)(T3)) *)

(* module Quadruple_with_collections(T1:S)(T2:S)(T3:S)(T4:S) = *)
(*   With_collections(Quadruple(T1)(T2)(T3)(T4)) *)

(* module Option_with_collections(T:S) = *)
(*   With_collections (Option(T)) *)

(* module List_with_collections(T:S) = *)
(*   With_collections (List(T)) *)

(* module Array_with_collections(T:S) = *)
(*   With_collections (Array(T)) *)
