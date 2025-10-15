module Ptmap_bench = Bench.Make (struct
  type kv = int * string
  type t = string Ptmap.t

  let make_kv kv = kv
  let name = "Ptmap"
  let of_seq = Some Ptmap.of_seq
end)

module CCIntMap_bench = Bench.Make (struct
  type kv = int * string
  type t = string CCIntMap.t

  let make_kv kv = kv
  let name = "CCIntMap"
  let of_seq = Some CCIntMap.of_seq
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
  let of_seq = Some M.of_seq
end)

let tests =
  [ Ptmap_bench.tests; CCIntMap_bench.tests; PatriciaTree_bench.tests ]
