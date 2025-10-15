module Ptmap_bench = Bench.Make (struct
  type kv = int * string
  type t = string Ptmap.t

  let make_kv kv = kv
  let name = "Ptmap"
  let empty = Ptmap.empty
  let add = fun t (k, v) -> Ptmap.add k v t
  let of_list _ = raise Bench.Unsupported
  let of_seq = Ptmap.of_seq
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
  let add _ _ = raise Bench.Unsupported
  let of_list = M.of_list
  let of_seq = M.of_seq
end)

let tests =
  [ Ptmap_bench.tests; CCIntMap_bench.tests; PatriciaTree_bench.tests ]
