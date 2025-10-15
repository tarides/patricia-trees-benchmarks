open QCheck

exception Unsupported
(** Raised by operations that are not supported by a given implementation. *)

type t = { op_name : string; test : Bechamel.Test.t }
(** Benchmarks defined by the functor [Make]. See {!merge} below. *)

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

  val empty : t

  (** Operations that are measured. Operations that are not supported raise
      [Unsupported]. *)

  val of_seq : kv Seq.t -> t
  val add : t -> kv -> t
end) : sig
  val tests : t list
end = struct
  open Bechamel

  let make_test op_name call =
    let name = Impl.name in
    { op_name; test = Test.make ~name (Staged.stage @@ call) }

  let random_kv_list = List.map Impl.make_kv random_key_value_list
  let random_kv_seq = List.to_seq random_kv_list
  let t_of_seq = make_test "of_seq" @@ fun () -> Impl.of_seq random_kv_seq

  let t_add =
    make_test "add" @@ fun () ->
    List.fold_left Impl.add Impl.empty random_kv_list

  let tests = [ t_of_seq; t_add ]
end

(** Group tests defined in [Make] by operations to allow comparing the result.
*)
let merge : t list list -> (string * Bechamel.Test.t) list =
 fun benchs ->
  let module SMap = Map.Make (String) in
  let by_op_name =
    List.fold_left
      (List.fold_left (fun acc test ->
           SMap.add_to_list test.op_name test.test acc))
      SMap.empty benchs
  in
  (* Use the first test list to keep the defined order. *)
  match benchs with
  | [] -> []
  | hd :: _ ->
      List.map
        (fun { op_name; _ } ->
          ( op_name,
            Bechamel.Test.make_grouped ~name:"" ~fmt:"%s%s"
              (SMap.find op_name by_op_name) ))
        hd
