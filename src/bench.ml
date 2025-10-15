open QCheck

exception Unsupported
(** Raised by operations that are not supported by a given implementation. *)

type t = { op_name : string; test : Bechamel.Test.t }
(** Benchmarks defined by the functor [Make]. See {!merge} below. *)

(** Pre-constructed test data. *)

let state = Random.get_state ()
let tree_size = 10000
let value_size = 100
let ordered_key_list = List.init tree_size (fun i -> i)
let random_key_list = List.init tree_size (fun _ -> Gen.int state)

let random_value_list =
  List.init tree_size (fun _ -> Gen.(string_size (return value_size) state))

let random_key_value_list = List.combine random_key_list random_value_list
let random_key_value_seq = List.to_seq random_key_value_list
let ordered_key_value_list = List.combine ordered_key_list random_value_list

let ordered_high_key_value_list =
  List.init tree_size (fun i -> (Int.max_int - i, string_of_int i))

let mixed_key_lists =
  let mk_mixed ratio =
    List.init tree_size (fun i ->
        match Gen.(option ~ratio int state) with Some r -> r | None -> i)
  in
  [
    (0, ordered_key_list);
    (1, mk_mixed 0.01);
    (5, mk_mixed 0.05);
    (25, mk_mixed 0.25);
    (100, random_key_list);
  ]

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

  val add : t -> kv -> t
  val of_list : kv list -> t
  val of_seq : kv Seq.t -> t
end) : sig
  val tests : t list
end = struct
  open Bechamel

  let make_test op_name call =
    { op_name; test = Test.make ~name:Impl.name (Staged.stage @@ call) }

  let make_indexed op_name ~fmt args call =
    {
      op_name;
      test =
        Test.make_indexed ~name:Impl.name ~fmt ~args (fun arg ->
            Staged.stage (call arg));
    }

  let random_kv_list = List.map Impl.make_kv random_key_value_list
  let random_kv_seq = List.to_seq random_kv_list
  let ordered_kv_list = List.map Impl.make_kv ordered_key_value_list
  let ordered_high_kv_list = List.map Impl.make_kv ordered_high_key_value_list

  let mixed_kv_lists =
    List.map
      (fun (i, l) -> (i, List.map (fun k -> Impl.make_kv (k, "")) l))
      mixed_key_lists

  let t_construct_pos_low_ordered =
    make_test
      "Construction with positive, <ordered keys close to '0' using 'add' and \
       'empty'."
    @@ fun () -> List.fold_left Impl.add Impl.empty ordered_kv_list

  let t_construct_pos_high_ordered =
    make_test
      "Construction with positive, >ordered keys close to 'max_int using 'add' \
       and 'empty'."
    @@ fun () -> List.fold_left Impl.add Impl.empty ordered_high_kv_list

  let t_construct_mixed =
    make_indexed
      "Construction with small positive unordered keys and part of random keys \
       a using 'add' and 'empty'."
      ~fmt:"%s (%d%% random)"
      (List.map fst mixed_kv_lists)
    @@ fun i () ->
    List.fold_left Impl.add Impl.empty (List.assoc i mixed_kv_lists)

  let t_of_list =
    make_test "Construction with 'of_list'" @@ fun () ->
    Impl.of_list random_kv_list

  let t_of_seq =
    make_test "Construction with 'of_seq" @@ fun () -> Impl.of_seq random_kv_seq

  let tests =
    [
      t_construct_pos_low_ordered;
      t_construct_pos_high_ordered;
      t_construct_mixed;
      t_of_list;
      t_of_seq;
    ]
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
