open Bechamel

module PatT = PatriciaTree.MakeMap (struct
  type t = int

  let to_int = Fun.id
end)

let pt_random_key_value_seq =
  let aux (k, v) = PatT.BaseMap.(KeyValue (k, Snd v)) in
  List.(to_seq @@ map aux Data.random_key_value_list)

let make_test name call = Test.make ~name (Staged.stage @@ call)

let test_ptmap_of_seq =
  make_test "Ptmap.of_seq" @@ fun () -> Ptmap.of_seq Data.random_key_value_seq

let test_containers_of_seq =
  make_test "CCIntMap.of_seq" @@ fun () ->
  CCIntMap.of_seq Data.random_key_value_seq

let test_patricia_tree_of_seq =
  make_test "Patricia_tree.of_seq" @@ fun () ->
  PatT.BaseMap.of_seq pt_random_key_value_seq

let tests =
  Test.make_grouped ~name:"building patrica trees" ~fmt:"%s %s"
    [ test_ptmap_of_seq; test_containers_of_seq; test_patricia_tree_of_seq ]
