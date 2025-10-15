(*************************************************************************)
(*  This file is part of Colibri2.                                       *)
(*                                                                       *)
(*  Copyright (C) 2014-2021                                              *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*                                                                       *)
(*  you can redistribute it and/or modify it under the terms of the GNU  *)
(*  Lesser General Public License as published by the Free Software      *)
(*  Foundation, version 2.1.                                             *)
(*                                                                       *)
(*  It is distributed in the hope that it will be useful,                *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*  GNU Lesser General Public License for more details.                  *)
(*                                                                       *)
(*  See the GNU Lesser General Public License version 2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).           *)
(*************************************************************************)

type 'a t = 'a array

let is_empty = function [||] -> true | _ -> false
let not_empty = function [||] -> false | _ -> true
let empty = [||]
let of_list = Array.of_list

let of_list_map ~f = function
  | [] -> empty
  | h :: l ->
      let len = List.length l in
      let a = Array.make (len + 1) (f h) in
      let rec fill i = function
        | [] -> ()
        | hd :: tl ->
            Array.unsafe_set a i (f hd);
            fill (i + 1) tl
      in
      fill 1 l;
      a

let of_array a = if Array.length a = 0 then empty else Array.copy a

let of_array_map ~f a =
  let l = Array.length a in
  if l = 0 then empty
  else
    let r = Array.make l (f a.(0)) in
    for i = 1 to l - 1 do
      r.(i) <- f a.(i)
    done;
    r

let to_list = Array.to_list
let to_seq = Base.Array.to_sequence
let length = Array.length

let equal cmp t1 t2 =
  Array.length t1 = Array.length t2
  &&
  let last = Array.length t1 - 1 in
  try
    for i = 0 to last do
      if not (cmp t1.(i) t2.(i)) then raise Exit
    done;
    true
  with Exit -> false

let compare cmp t1 t2 =
  let lt1 = Array.length t1 in
  let c = compare lt1 (Array.length t2) in
  if c <> 0 then c
  else
    match lt1 with
    | 0 -> 0
    | 1 -> cmp t1.(0) t2.(0)
    | 2 ->
        let c = cmp t1.(0) t2.(0) in
        if c <> 0 then c else cmp t1.(1) t2.(1)
    | 3 ->
        let c = cmp t1.(0) t2.(0) in
        if c <> 0 then c
        else
          let c = cmp t1.(1) t2.(1) in
          if c <> 0 then c else cmp t1.(2) t2.(2)
    | 4 ->
        let c = cmp t1.(0) t2.(0) in
        if c <> 0 then c
        else
          let c = cmp t1.(1) t2.(1) in
          if c <> 0 then c
          else
            let c = cmp t1.(2) t2.(2) in
            if c <> 0 then c else cmp t1.(3) t2.(3)
    | _ ->
        let rec aux t1 t2 = function
          | -1 -> 0
          | i ->
              let c = cmp t1.(i) t2.(i) in
              if c <> 0 then c else aux t1 t2 (i - 1)
        in
        aux t1 t2 (lt1 - 1)

let hash h t =
  let last = Array.length t - 1 in
  let c = ref last in
  for i = 0 to last do
    c := Hashcons.combine (h t.(i)) !c
  done;
  !c

let hash_fold_t h s t =
  let last = Array.length t - 1 in
  let c = ref s in
  for i = 0 to last do
    c := h !c t.(i)
  done;
  !c

let get = Array.get
let iter ~f a = Array.iter f a
let iteri ~f a = Array.iteri f a
let fold ~f ~init a = Array.fold_left f init a

let foldi a ~init:x ~f =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f i !r (Array.unsafe_get a i)
  done;
  !r

let foldi_non_empty_exn ~init ~f a =
  if Array.length a = 0 then
    invalid_arg "IArray.fold_non_empty_exn: empty array";
  let r = ref (init (Array.unsafe_get a 0)) in
  for i = 1 to Array.length a - 1 do
    r := f i !r (Array.unsafe_get a i)
  done;
  !r

let fold2_exn ~init ~f a1 a2 =
  if Array.length a1 <> Array.length a2 then
    invalid_arg "IArray.fold2_exn: different length";
  let r = ref init in
  for i = 0 to Array.length a1 - 1 do
    r := f !r a1.(i) a2.(i)
  done;
  !r

let for_alli ~f a1 =
  let exception False in
  try
    for i = 0 to Array.length a1 - 1 do
      if not (f i a1.(i)) then raise False
    done;
    true
  with False -> false

let for_alli_non_empty_exn ~init ~f a =
  if Array.length a = 0 then
    invalid_arg "IArray.for_alli_non_empty_exn: empty array";
  let first = init a.(0) in
  let exception False in
  try
    for i = 1 to Array.length a - 1 do
      if not (f i first a.(i)) then raise False
    done;
    true
  with False -> false

let for_all2_exn ~f a1 a2 =
  if Array.length a1 <> Array.length a2 then
    invalid_arg "IArray.fold2_exn: different length";
  let exception False in
  try
    for i = 0 to Array.length a1 - 1 do
      if not (f a1.(i) a2.(i)) then raise False
    done;
    true
  with False -> false

let map ~f a = Array.map f a
let map2_exn ~f a b = Array.map2 f a b
let pp ?(sep = Fmt.comma) p fmt a = Fmt.array ~sep p fmt a

let extract1_exn = function
  | [| a |] -> a
  | _ -> invalid_arg "IArray not of size 1"

let extract2_exn = function
  | [| a; b |] -> (a, b)
  | _ -> invalid_arg "IArray not of size 2"

let extract3_exn = function
  | [| a; b; c |] -> (a, b, c)
  | _ -> invalid_arg "IArray not of size 3"

let extract4_exn = function
  | [| a; b; c; d |] -> (a, b, c, d)
  | _ -> invalid_arg "IArray not of size 4"

let mk1 x = [| x |]
let mk2 x y = [| x; y |]
