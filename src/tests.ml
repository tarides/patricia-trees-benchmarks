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
  open Colibri2_popop_lib

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

module Colibri_mergemap_bench = Bench.Make (struct
  open Colibri2_popop_lib

  module Key = struct
    type t = int

    let hash = Int.hash
    let equal = Int.equal
    let compare = Int.compare
    let pp = Pp.int
    let hash_fold_t _ = assert false (* Not used *)
  end

  module M = Mergemap.Make (Key)

  type kv = M.key * string
  type t = string M.data M.t

  let make_kv = Fun.id
  let name = "Colibri2/Mergemap"
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
  open Colibri2_popop_lib

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
  open Colibri2_popop_lib

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

module Colibri_map_hetero_bench = Bench.Make (struct
  open Colibri2_stdlib.Std

  module Key = struct
    type _ t = Str : int -> string t [@@unboxed]

    let tag (type a) : a t -> int = fun (Str tag) -> tag

    let equal (type a b) (a : a t) (b : b t) : (a, b) Poly.iseq =
      let Str a, Str b = (a, b) in
      if a = b then Poly.Eq else Poly.Neq
  end

  module M = Colibri2_stdlib.Map_hetero.MakeR (Key)

  type kv = string Key.t * string
  type t = string M.t

  let make_kv (k, v) = (Key.Str k, v)
  let name = "Colibri_int_hetero"
  let empty = M.empty
  let add t (k, v) = M.add k v t
  let of_list _ = raise Bench.Unsupported
  let of_seq _ = raise Bench.Unsupported

  let union =
    let union _ a _ = Some a in
    M.union { M.union }

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
    PatriciaTree_bench.tests;
    HashconsedPatriciaTree_bench.tests;
    Colibri_map_hetero_bench.tests;
    Colibri_intmap_bench.tests;
    Colibri_intmap_hash_consed_bench.tests;
    Colibri_intmap_hetero_bench.tests;
    Colibri_mergemap_bench.tests;
    Frama_C_intmap_bench.tests;
    Frama_C_idxmap_bench.tests;
    Frama_C_mergemap_bench.tests;
  ]
