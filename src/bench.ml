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

let random_key_list ?(size = tree_size) () =
  List.init size (fun _ -> Gen.int state)

let key_value_of_key k = (k, string_of_int k)

let random_key_value_list ?size () =
  List.map key_value_of_key (random_key_list ?size ())

let random_key_value_seq = List.to_seq (random_key_value_list ())
let ordered_key_value_list = List.map key_value_of_key ordered_key_list

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
    (100, random_key_list ());
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
  val union : t -> t -> t

  val merge :
    (int -> string option -> string option -> string option) -> t -> t -> t

  val inter : t -> t -> t
  val diff : t -> t -> t
end) : sig
  val tests : t list
end = struct
  open Bechamel

  let make_test op_name call =
    { op_name; test = Test.make ~name:Impl.name (Staged.stage @@ call) }

  (* let make_tests op_name ?(fmt : Test.fmt_grouped = "%s.%s") tests = *)
  (*   let tests = *)
  (*     List.map (fun (name, call) -> Test.make ~name (Staged.stage call)) tests *)
  (*   in *)
  (*   { op_name; test = Test.make_grouped ~name:Impl.name ~fmt tests } *)

  let make_indexed op_name ~fmt args call =
    let f arg = Staged.stage (call arg) in
    { op_name; test = Test.make_indexed ~name:Impl.name ~fmt ~args f }

  let mk_random_kv_list ?size () =
    List.map Impl.make_kv (random_key_value_list ?size ())

  let random_kv_list = mk_random_kv_list ()
  let random_kv_seq = List.to_seq (mk_random_kv_list ())
  let ordered_kv_list = List.map Impl.make_kv ordered_key_value_list
  let ordered_high_kv_list = List.map Impl.make_kv ordered_high_key_value_list
  let mk_tree = List.fold_left Impl.add Impl.empty

  let random_trees =
    Array.init 10 (fun _ -> mk_tree (mk_random_kv_list ~size:1000 ()))

  (* Quickly return a tree from a collection of pre-built trees. These trees
     are smaller than [tree_size]. *)
  let random_tree () =
    random_trees.(Gen.int_bound (Array.length random_trees - 1) state)

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

  let t_union =
    make_test "Set operations: union" @@ fun () ->
    Impl.union (random_tree ()) (random_tree ())

  let t_merge =
    make_test "Set operations: merge (left biased)" @@ fun () ->
    Impl.merge
      (fun _ a b -> if Option.is_none a then b else a)
      (random_tree ()) (random_tree ())

  let t_inter =
    make_test "Set operations: inter" @@ fun () -> Impl.inter (random_tree ()) (random_tree ())

  let t_diff =
    make_test "Set operations: diff" @@ fun () -> Impl.diff (random_tree ()) (random_tree ())

  let tests =
    [
      t_construct_pos_low_ordered;
      t_construct_pos_high_ordered;
      t_construct_mixed;
      t_of_list;
      t_of_seq;
      t_union;
      t_merge;
      t_inter;
      t_diff;
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
