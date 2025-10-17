(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Stdlib.Hashtbl

module type S = sig
  include S
  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val find_default: default:'a -> 'a t -> key  -> 'a

  val find_def: 'a t -> key  -> 'a -> 'a
  [@@deprecated "Use find_default instead."]

  val memo: 'a t -> key -> (key -> 'a) -> 'a

end

module Make(H: HashedType) : S with type key = H.t  = struct

  include Make(H)

  let bindings_sorted ?(cmp=Stdlib.compare) h =
    to_seq h |> List.of_seq |> List.fast_sort (fun (k1,_) (k2,_) -> cmp k1 k2)

  let fold_sorted ?(cmp=Stdlib.compare) f h acc =
    let l = bindings_sorted ~cmp h in
    List.fold_left (fun acc (k,v) -> f k v acc) acc l

  let iter_sorted ?cmp f h =
    fold_sorted ?cmp (fun k v () -> f k v) h ()

  let fold_sorted_by_entry ~cmp f h acc =
    let l = to_seq h |> List.of_seq |> List.fast_sort cmp in
    List.fold_left (fun acc (k,v) -> f k v acc) acc l

  let iter_sorted_by_entry ~cmp f h =
    fold_sorted_by_entry ~cmp (fun k v () -> f k v) h  ()

  let fold_sorted_by_value ~cmp f h acc =
    fold_sorted_by_entry ~cmp:(fun (_ka,va) (_kb,vb) -> cmp va vb) f h acc

  let iter_sorted_by_value ~cmp f h =
    iter_sorted_by_entry ~cmp:(fun (_ka,va) (_kb,vb) -> cmp va vb) f h

  let find_default ~default h k =
    match find_opt h k with
    | None -> default
    | Some v -> v

  let find_def h k default = find_default ~default h k

  let memo tbl k f =
    try find tbl k
    with Not_found ->
      let v = f k in
      add tbl k v;
      v
end
