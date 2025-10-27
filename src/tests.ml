module Ptmap_bench = Bench.Make (struct
  type kv = int * string
  type t = string Ptmap.t

  let make_kv kv = kv
  let name = "Ptmap"
  let empty = Ptmap.empty
  let add = fun t (k, v) -> Ptmap.add k v t
  let of_list _ = raise Bench.Unsupported
  let of_seq = Ptmap.of_seq
  let union = Ptmap.union (fun _ a _ -> Some a)
  let merge = Ptmap.merge
  let inter _ _ = raise Bench.Unsupported
  let diff _ _ = raise Bench.Unsupported
end)

module CCIntMap_bench = Bench.Make (struct
  type kv = int * string
  type t = string CCIntMap.t

  let make_kv kv = kv
  let name = "CCIntMap"
  let empty = CCIntMap.empty
  let add t (k, v) = CCIntMap.add k v t
  let of_list = CCIntMap.of_list
  let of_seq = CCIntMap.of_seq
  let union = CCIntMap.union (fun _ a _ -> a)

  let merge f a b =
    (* The merge function has an unusual signature, which might slightly
       disadvantage it in benchmarks. *)
    let f k = function
      | `Left a -> f k (Some a) None
      | `Right b -> f k None (Some b)
      | `Both (a, b) -> f k (Some a) (Some b)
    in
    CCIntMap.merge ~f a b

  let inter a b = CCIntMap.inter (fun _ a _ -> a) a b
  let diff _ _ = raise Bench.Unsupported
end)

module Dmap_bench = Bench.Make (struct
  module Key = struct
    type _ t = Str : int -> string t [@@unboxed]

    let compare (type a b) (a : a t) (b : b t) : (a, b) Dmap.cmp =
      let Str a, Str b = (a, b) in
      if a < b then Dmap.Lt else if a > b then Gt else Eq
  end

  module M = Dmap.Make (Key)

  type kv = M.binding
  type t = M.t

  let make_kv (k, v) = M.Binding (Key.Str k, v)
  let name = "Dmap"
  let empty = M.empty
  let add t (M.Binding (k, v)) = M.add k v t
  let of_list _ = raise Bench.Unsupported
  let of_seq = M.of_seq

  let union =
    let union_fun _ a _ = Some a in
    M.union { M.union_fun }

  let merge (f : int -> string option -> string option -> string option) a b =
    let merge_fun (type a) (k : a Key.t) (a : a option) (b : a option) :
        a option =
      match k with Key.Str k -> f k a b | _ -> .
    in
    M.merge { M.merge_fun } a b

  let inter _ _ = raise Bench.Unsupported
  let diff _ _ = raise Bench.Unsupported
end)

module Gmap_bench = Bench.Make (struct
  module Key = struct
    type _ t = Str : int -> string t [@@unboxed]

    let compare (type a b) (a : a t) (b : b t) : (a, b) Gmap.Order.t =
      let open Gmap.Order in
      let Str a, Str b = (a, b) in
      if a < b then Lt else if a > b then Gt else Eq
  end

  module M = Gmap.Make (Key)

  type kv = M.b
  type t = M.t

  let make_kv (k, v) = M.B (Key.Str k, v)
  let name = "Gmap"
  let empty = M.empty
  let add t (M.B (k, v)) = M.add k v t
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported

  let union =
    let f _ a _ = Some a in
    M.union { M.f }

  let merge (f : int -> string option -> string option -> string option) a b =
    let f (type a) (k : a Key.t) (a : a option) (b : a option) : a option =
      match k with Key.Str k -> f k a b | _ -> .
    in
    M.merge { M.f } a b

  let inter _ _ = raise Bench.Unsupported
  let diff _ _ = raise Bench.Unsupported
end)

module Hmap_bench = Bench.Make (struct
  type kv = Hmap.binding
  type t = Hmap.t

  let make_kv (_k, v) = Hmap.B (Hmap.Key.create (), v)
  let name = "Hmap"
  let empty = Hmap.empty
  let add t (Hmap.B (k, v)) = Hmap.add k v t
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported
  let union _ _ = raise Bench.Unsupported
  let merge _ _ _ = raise Bench.Unsupported
  let inter _ _ = raise Bench.Unsupported
  let diff _ _ = raise Bench.Unsupported
end)

module PatriciaTree_bench = Bench.Make (struct
  module M = PatriciaTree.MakeMap (struct
    type t = int

    let to_int = Fun.id
  end)

  type kv = int * string
  type t = string M.t

  let make_kv kv = kv
  let name = "Codex/BaseMap"
  let empty = M.empty
  let add t (k, v) = M.add k v t
  let of_list = M.of_list
  let of_seq = M.of_seq
  let union a b = M.idempotent_union (fun _ a _ -> a) a b
  let merge f a b = M.slow_merge f a b
  let inter a b = M.idempotent_inter (fun _ a _ -> a) a b
  let diff a b = M.symmetric_difference (fun _ _ _ -> None) a b
end)

module HashconsedPatriciaTree_bench = Bench.Make (struct
  module M =
    PatriciaTree.MakeHashconsedMap
      (struct
        type t = int

        let to_int = Fun.id
      end)
      (struct
        type _ t = string

        let hash = String.hash
        let polyeq = String.equal
      end)
      ()

  type kv = int * string
  type t = string M.t

  let make_kv kv = kv
  let name = "Codex/HashConsedMap"
  let empty = M.empty
  let add t (k, v) = M.add k v t
  let of_list = M.of_list
  let of_seq = M.of_seq
  let union a b = M.idempotent_union (fun _ a _ -> a) a b
  let merge f a b = M.slow_merge f a b
  let inter a b = M.idempotent_inter (fun _ a _ -> a) a b
  let diff a b = M.symmetric_difference (fun _ _ _ -> None) a b
end)

module Colibri_intmap_bench = Bench.Make (struct
  open Popop_lib

  module Key = struct
    type t = int

    let tag = Fun.id
    let equal = Int.equal
    let pp = Pp.int
  end

  module A = Intmap.Make (Key)
  module M = A.NT

  type kv = M.key * string
  type t = string M.data M.t

  let make_kv = Fun.id
  let name = "Colibri2/Intmap"
  let empty = M.empty
  let add m (k, v) = M.add k v m
  let of_list = M.of_list
  let of_seq _ = raise Bench.Unsupported
  let union = M.union (fun _ a _ -> Some a)
  let merge f = M.union_merge (fun k a b -> f k a (Some b))
  let inter = M.inter (fun _ a _ -> Some a)
  let diff = M.diff (fun _ a _ -> Some a)
end)

module Colibri_intmap_hash_consed_bench = Bench.Make (struct
  open Popop_lib

  module Key = struct
    type t = int

    let tag = Fun.id
    let equal = Int.equal
    let pp = Pp.int
  end

  module Data = struct
    type t = string

    let hash = String.hash
    let equal = String.equal
    let pp = Pp.string
  end

  module A = Intmap.Make (Key)
  module M = A.Make (Data)

  type kv = M.key * string
  type t = string M.data M.t

  let make_kv = Fun.id
  let name = "Colibri2/Intmap-hashconsed"
  let empty = M.empty
  let add m (k, v) = M.add k v m
  let of_list = M.of_list
  let of_seq _ = raise Bench.Unsupported
  let union = M.union (fun _ a _ -> Some a)
  let merge f = M.union_merge (fun k a b -> f k a (Some b))
  let inter = M.inter (fun _ a _ -> Some a)
  let diff = M.diff (fun _ a _ -> Some a)
end)

module Colibri_intmap_hetero_bench = Bench.Make (struct
  open Popop_lib

  module M =
    Intmap_hetero.Make1
      (struct
        type 'a t = int
      end)
      (struct
        type ('a, 'b) t = 'b
      end)

  type kv = int * string
  type t = (int, string) M.data M.t

  let make_kv (k, v) = (k, v)
  let name = "Colibri2/Intmap_hetero"
  let empty = M.empty
  let add m (k, v) = M.add k v m
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported
  let union _ _ = raise Bench.Unsupported
  let merge _ _ _ = raise Bench.Unsupported
  let inter _ _ = raise Bench.Unsupported
  let diff _ _ = raise Bench.Unsupported
end)

module Frama_C_intmap_bench = Bench.Make (struct
  open Framac_intmap.Intmap

  type kv = int * string
  type nonrec t = string t

  let make_kv = Fun.id
  let name = "FramaC/Intmap"
  let empty = empty
  let add m (k, v) = add k v m
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported
  let union = union (fun _ x _ -> x)
  let merge = merge
  let inter = inter (fun _ x _ -> x)
  let diff = diffq (fun _ _ _ -> None)
end)

module Frama_C_idxmap_bench = Bench.Make (struct
  open Framac_idxmap.Idxmap

  module M = Make (struct
    type t = int

    let id = Fun.id
  end)

  type kv = int * string
  type t = string M.t

  let make_kv = Fun.id
  let name = "FramaC/Idxmap"
  let empty = M.empty
  let add m (k, v) = M.add k v m
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported
  let union = M.union (fun _ x _ -> x)
  let merge = M.merge
  let inter = M.inter (fun _ x _ -> x)
  let diff = M.diffq (fun _ _ _ -> None)
end)

module Frama_C_mergemap_bench = Bench.Make (struct
  open Framac_mergemap.Mergemap

  module M = Make (struct
    type t = int

    let hash = Fun.id
    let equal = Int.equal
    let compare = Int.compare
  end)

  type kv = int * string
  type t = string M.t

  let make_kv = Fun.id
  let name = "FramaC/Mergemap"
  let empty = M.empty
  let add m (k, v) = M.add k v m
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported
  let union = M.union (fun _ x _ -> x)
  let merge = M.merge
  let inter = M.inter (fun _ x _ -> x)
  let diff = M.diffq (fun _ _ _ -> None)
end)

let tests =
  [
    Ptmap_bench.tests;
    CCIntMap_bench.tests;
    Dmap_bench.tests;
    Gmap_bench.tests;
    Hmap_bench.tests;
    PatriciaTree_bench.tests;
    HashconsedPatriciaTree_bench.tests;
    Colibri_intmap_bench.tests;
    Colibri_intmap_hash_consed_bench.tests;
    Frama_C_intmap_bench.tests;
    Frama_C_idxmap_bench.tests;
    Frama_C_mergemap_bench.tests;
  ]
