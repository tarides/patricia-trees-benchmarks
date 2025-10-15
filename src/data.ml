open QCheck

(** Pre-constructed test data. *)

let state = Random.get_state ()
let tree_size = 10000
let value_size = 100
let random_key_list = List.init tree_size (fun _ -> Gen.int state)

let random_value_list =
  List.init tree_size (fun _ -> Gen.(string_size (return value_size) state))

let random_key_value_list = List.combine random_key_list random_value_list
let random_key_value_seq = List.to_seq random_key_value_list

(** Defines benchmarks for an abstract implementation. This ensures that the
    output is consistent and the results are comparable. See the instances in
    [tests.ml]. *)
module Make (Impl : sig
  type kv
  (** Key and value pair *)

  type t
  (** Data structure with keys of type [int] and values of type [string]. *)

  val make_kv : int * string -> kv
  (** Pair a key and a value, for implementations that require wrapping the keys
      and/or the values, for example for heterogeneous maps. *)

  val name : string
  (** Implementation name used to name benchmarks. *)

  (** Operations that are measured. Operations that are not supported are equal
      to [None]. *)

  val of_seq : (kv Seq.t -> t) option
end) : sig
  val tests : Bechamel.Test.t
end = struct
  open Bechamel

  let make_test name impl call =
    match impl with
    | Some impl -> Test.make ~name (Staged.stage @@ call impl)
    | None ->
        (* Use [make_indexed] with an empty list to show unsupported operations
           in the output. *)
        Test.make_indexed ~name ~fmt:"unsupported %s %d" ~args:[] (fun _ ->
            assert false)

  let random_kv_list = List.map Impl.make_kv random_key_value_list
  let random_kv_seq = List.to_seq random_kv_list
  let t_of_seq = make_test "of_seq" Impl.of_seq @@ fun f () -> f random_kv_seq
  let tests = Test.make_grouped ~name:Impl.name ~fmt:"%s %s" [ t_of_seq ]
end
