open QCheck

let state = Random.get_state ()
let tree_size = 10000
let value_size = 100
let random_key_list = List.init tree_size (fun _ -> Gen.int state)

let random_value_list =
  List.init tree_size (fun _ -> Gen.(string_size (return value_size) state))

let random_key_value_list = List.combine random_key_list random_value_list
let random_key_value_seq = List.to_seq random_key_value_list
