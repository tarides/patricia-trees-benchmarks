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
  module PatT = PatriciaTree.MakeMap (struct
    type t = int

    let to_int = Fun.id
  end)

  module M = PatT.BaseMap

  type kv = string M.key_value_pair
  type t = string M.t

  let make_kv (k, v) = M.(KeyValue (k, Snd v))
  let name = "PatriciaTree"
  let empty = M.empty
  let add t (M.KeyValue (k, v)) = M.add k v t
  let of_list = M.of_list
  let of_seq = M.of_seq
  let polyunion : (_, _, _) M.polyunion = { M.f = (fun _ a _ -> a) }
  let union a b = M.idempotent_union polyunion a b
  let merge _ _ _ = raise Bench.Unsupported
  let polyinter : (_, _, _) M.polyinter = { M.f = (fun _ a _ -> a) }
  let inter a b = M.idempotent_inter polyinter a b
  let polydiff : (_, _, _) M.polyinterfilter = { M.f = (fun _ _ _ -> None) }
  let diff a b = M.symmetric_difference polydiff a b
end)

let tests =
  [ Ptmap_bench.tests; CCIntMap_bench.tests; PatriciaTree_bench.tests ]
