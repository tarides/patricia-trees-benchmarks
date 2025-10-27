(*************************************************************************)
(*  This file is part of Colibri2.                                       *)
(*                                                                       *)
(*  Copyright (C) 2017-2021                                              *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
(*    CNRS  (Centre national de la recherche scientifique)               *)
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

include Std_sig

let debug =
  Debug.register_info_flag ~desc:"information about algebraic numbers"
    "algebraic-number"

let nnil = function [] -> false | _ :: _ -> true

module Poly = struct
  type (_, _, _) t =
    | Eq : ('a, 'a, [< `Eq | `IsEq | `Ord ]) t
    | Neq : (_, _, [ `IsEq ]) t
    | Gt : (_, _, [ `Ord ]) t
    | Lt : (_, _, [ `Ord ]) t

  type ('a, 'b) eq = ('a, 'b, [ `Eq ]) t
  type ('a, 'b) iseq = ('a, 'b, [ `IsEq ]) t
  type ('a, 'b) ord = ('a, 'b, [ `Ord ]) t

  exception NotEq

  let iseq (type a b) : (a, b, [< `Eq | `IsEq | `Ord ]) t -> (a, b) iseq =
    function
    | Eq -> Eq
    | _ -> Neq

  let eq (type a b) : (a, b, [< `Eq | `IsEq | `Ord ]) t -> (a, b) eq = function
    | Eq -> Eq
    | _ -> raise NotEq
end

module Goption = struct
  type (_, _) t = Some : 'a -> ('a, [ `Some ]) t | None : ('a, [ `None ]) t
end

module Z = struct
  module Arg = struct
    include Z

    let hash_fold_t s (t : Z.t) = Base.Hash.fold_int s (Z.hash t)

    let pp_hexa fmt z =
      if Z.equal z Z.zero then Fmt.pf fmt "0"
      else
        let p = Z.trailing_zeros z in
        let z = Z.shift_right z p in
        Fmt.pf fmt "%sp%i" (Z.format "%x" z) p

    let pp fmt z =
      if Int.(Z.numbits z < 16) then Z.pp_print fmt z else pp_hexa fmt z
  end

  include Arg
  include Colibri2_popop_lib.Popop_stdlib.MkDatatype (Arg)

  let is_zero q = Z.sign q = 0

  (** maximum size in word for Z.t before we stop the propagation *)
  let max_size = 20

  (** maximum size in bit for Z.t before we stop the propagation *)
  let max_size_bit = Int.(max_size * Sys.word_size)

  let big_number z =
    let z = Obj.repr z in
    (not (Obj.is_int z)) && Obj.size z > max_size

  let gen = QCheck2.Gen.map Z.of_int QCheck2.Gen.small_signed_int
end

(* Extending Q module from Zarith *)
module Z_Q = struct
  module Arg = struct
    include Q (* Module from Zarith *)

    module Hash = struct
      type t = Q.t = { num : Z.t; den : Z.t } [@@deriving hash]
    end

    let hash = Hash.hash
    let hash_fold_t = Hash.hash_fold_t

    let pp fmt q =
      Format.(
        match Q.classify q with
        | Q.ZERO -> char fmt '0'
        | Q.INF -> string fmt "+∞"
        | Q.MINF -> string fmt "-∞"
        | Q.UNDEF -> string fmt "!undef!"
        | Q.NZERO ->
            if Z.equal Z.one q.den then Z.pp fmt q.num
            else Fmt.pf fmt "%a/%a(~%.3f)" Z.pp q.num Z.pp q.den (Q.to_float q))
  end

  include Arg
  include Colibri2_popop_lib.Popop_stdlib.MkDatatype (Arg)

  let two = Q.of_int 2
  let ge = Q.geq
  let le = Q.leq

  let of_string_decimal =
    let dec_mark, decimal =
      let open Re in
      (* [+-]?[0-9]+([.][0-9]* )? *)
      let dec_mark, dec_part = mark (seq [ char '.'; group @@ rep digit ]) in
      ( dec_mark,
        compile @@ seq [ bos; opt (set "+-"); rep1 digit; opt @@ dec_part; eos ]
      )
    in
    fun s ->
      match Re.exec_opt decimal s with
      | None -> invalid_arg "of_string_decimal: bad format"
      | Some g ->
          if Re.Mark.test g dec_mark then (
            let s2 = Bytes.make Int.(String.length s - 1) '?' in
            let dec_start, dec_stop = Re.Group.offset g 1 in
            let dec_len = Int.(dec_stop - dec_start) in
            Bytes.blit_string s 0 s2 0 Int.(dec_start - 1);
            if not (Int.equal dec_len 0) then
              Bytes.blit_string s dec_start s2 Int.(dec_start - 1) dec_len;
            let n = Z.of_string (Bytes.unsafe_to_string s2) in
            if Int.equal dec_len 0 then Q.of_bigint n
            else Q.make n (Z.pow (Z.of_int 10) dec_len))
          else Q.of_bigint (Z.of_string s)

  let is_integer q = CCEqual.physical Z.one q.Q.den

  let is_unsigned_integer size q =
    is_integer q && Int.(Z.sign q.Q.num >= 0) && Int.(Z.numbits q.Q.num <= size)

  let floor x = if is_integer x then x else Q.of_bigint (Z.fdiv x.Q.num x.Q.den)
  let ceil x = if is_integer x then x else Q.of_bigint (Z.cdiv x.Q.num x.Q.den)

  let truncate d =
    if is_integer d then d else if Int.(Q.sign d > 0) then floor d else ceil d

  let div x y =
    assert (not (Q.equal Q.zero y));
    Q.div x y

  let make x y =
    assert (not (Z.equal Z.zero y));
    Q.make x y

  let div_e a b =
    let s = Q.sign b in
    let d = div a b in
    if Int.(s > 0) then floor d else ceil d

  let div_t a b = truncate (div a b)
  let div_f a b = floor (div a b)
  let mod_e a b = Q.sub a (Q.mul (div_e a b) b)
  let mod_t a b = Q.sub a (Q.mul (div_t a b) b)
  let mod_f a b = Q.sub a (Q.mul (div_f a b) b)
  let is_zero c = Int.equal (Q.sign c) 0
  let none_zero c = if is_zero c then None else Some c
  let is_not_zero c = not (Int.equal (Q.sign c) 0)

  let gen =
    let open QCheck2.Gen in
    let+ num = small_signed_int and+ den = small_nat in
    Q.make (Z.of_int num) (Z.of_int Int.(den + 1))

  let pow q n =
    if Int.(n < 0) then (
      assert (Int.(Q.sign q <> 0));
      Q.make (Z.pow q.den Int.(-n)) (Z.pow q.num Int.(-n)))
    else Q.make (Z.pow q.num n) (Z.pow q.den n)

  let gcd a b =
    (* Q.make (Z.gcd (Z.mul a.num b.den) (Z.mul b.num a.den)) (Z.mul a.den b.den) *)
    { num = Z.gcd a.num b.num; den = Z.lcm a.den b.den }

  let gcdext a b =
    let gcd, u, v = Z.gcdext (Z.mul a.num b.den) (Z.mul b.num a.den) in
    (Q.make gcd (Z.mul a.den b.den), u, v)

  let lcm a b =
    (* Q.make (Z.lcm (Z.mul a.num b.den) (Z.mul b.num a.den)) (Z.mul a.den b.den) *)
    { num = Z.lcm a.num b.num; den = Z.gcd a.den b.den }

  let divisible a b = Z.divisible a.num b.num && Z.divisible b.den a.den

  let divexact a b =
    (* a.num / a.den * b.den / a.den *)
    Z.mul (Z.divexact a.num b.num) (Z.divexact b.den a.den)

  let round_down_to a m =
    let q' = floor (Q.div a m) in
    Q.mul q' m

  let round_up_to a m =
    let q' = ceil (Q.div a m) in
    Q.mul q' m

  let big_number a = Z.big_number a.num || Z.big_number a.den

  let smaller_den den =
    let s = Z.numbits den in
    let remaining =
      if Int.(s > Z.max_size_bit) then Int.(Z.max_size_bit / 4) else Int.(s / 4)
    in
    if Int.equal remaining 0 then Z.one
    else Z.shift_right den Int.(s - remaining)

  let round_down_to_small_number x =
    if big_number x then
      round_down_to x { num = Z.one; den = smaller_den x.den }
    else x

  let round_up_to_small_number x =
    if big_number x then round_up_to x { num = Z.one; den = smaller_den x.den }
    else x

  let to_q q = q
  let of_q q = q
  let to_z q = q.num
  let _ = to_z
end

(* module Q from Zarith with 2^p for big p *)
module Q_P = struct
  let min_p = 63

  module Arg = struct
    type t = { num : Z.t; den : Z.t; p : Base.Int.t } [@@deriving hash]

    let invariant x =
      if Z.equal x.num Z.zero then Int.(x.p = 0) && Z.equal x.den Z.one
      else
        (not (Z.equal x.den Z.zero))
        && (x.p = 0 || Int.abs x.p >= min_p)
        &&
        let p_num = Z.trailing_zeros x.num in
        let p_den = Z.trailing_zeros x.den in
        if x.p <> 0 then p_num = 0 && p_den = 0
        else (p_num = 0 || p_den = 0) && p_num < min_p && p_den < min_p

    let check_invariant x =
      if false then assert (invariant x);
      x

    let of_small_num num = check_invariant @@ { num; den = Z.one; p = 0 }
    let zero = of_small_num Z.zero
    let one = of_small_num Z.one
    let minus_one = of_small_num Z.minus_one

    let to_q x : Z_Q.t =
      if Int.(x.p = 0) then { num = x.num; den = x.den }
      else if Int.(x.p < 0) then
        { num = x.num; den = Z.shift_left x.den Int.(-x.p) }
      else { num = Z.shift_left x.num x.p; den = x.den }

    let of_q (x : Z_Q.t) : t =
      assert (not (CCEqual.physical Z.zero x.den));
      check_invariant
      @@
      let id () = { num = x.num; den = x.den; p = 0 } in
      if CCEqual.physical x.num Z.zero then zero
      else
        let p = Z.trailing_zeros x.num in
        if Int.(p = 0) then
          let p = Z.trailing_zeros x.den in
          if Int.(p < min_p) then id ()
          else { num = x.num; den = Z.shift_right x.den p; p = Int.(-p) }
        else if Int.(p < min_p) then id ()
        else { num = Z.shift_right x.num p; den = x.den; p }

    let unary_test f =
      let test =
        QCheck2.Test.make ~count:100 ~print:Z_Q.to_string Z_Q.gen (fun x ->
            f x;
            true)
      in
      QCheck2.Test.check_exn test

    let%test_unit "of_q" =
      unary_test (fun v ->
          let v' = to_q (of_q v) in
          if not (Z_Q.equal v v') then
            QCheck2.Test.fail_reportf "becomes %a" Z_Q.pp v')

    let norm_num x y =
      if x.p = y.p then (x.p, x.num, y.num)
      else if x.p < y.p then (x.p, x.num, Z.shift_left y.num (y.p - x.p))
      else (y.p, Z.shift_left x.num (x.p - y.p), y.num)

    let compare x y =
      let compare_q x_num x_den y_num y_den =
        if Z.equal x_den y_den then Z.compare x_num y_num
        else Z.compare (Z.mul x_num y_den) (Z.mul y_num x_den)
      in
      if x.p = y.p then compare_q x.num x.den y.num y.den
      else
        let _p, x_num, y_num = norm_num x y in
        compare_q x_num x.den y_num y.den

    let equal x y = x.p = y.p && Z.equal x.num y.num && Z.equal x.den y.den

    let pp fmt q =
      if false then
        Fmt.pf fmt "{num=%a;den=%a;p=%i}(%a)" Z.pp_print q.num Z.pp_print q.den
          q.p Z_Q.pp (to_q q)
      else if false then Z_Q.pp fmt (to_q q)
      else
        let pp_z fmt z =
          if Int.(Z.numbits z < 16) then Z.pp_print fmt z
          else Fmt.pf fmt "%s" (Z.format "%x" z)
        in
        let pp_p fmt p = (* if p <> 0 then *) Fmt.pf fmt "p%i" p in
        if Z.equal Z.one q.den then Fmt.pf fmt "%a%a" pp_z q.num pp_p q.p
        else
          Fmt.pf fmt "%a%a/%a%a(~%.3f)" pp_z q.num pp_p (Int.max 0 q.p) pp_z
            q.den pp_p
            Int.(max 0 (-q.p))
            (Q.to_float (to_q q))

    let compare x y =
      (* Fmt.epr "Ici(%a,%a)@." pp x pp y; *)
      let r = compare x y in
      (* let r' = Z_Q.compare (to_q x) (to_q y) in
      assert (r = r'); *)
      r
  end

  include Arg
  include Colibri2_popop_lib.Popop_stdlib.MkDatatype (Arg)

  let reduce n d =
    let g = Z.gcd n d in
    if CCEqual.physical g Z.one then (n, d) else (Z.divexact n g, Z.divexact d g)

  let mul_2p x p =
    if p = 0 then x
    else if p < 0 then Z.shift_right x (-p)
    else Z.shift_left x p

  let reduce_p reduce num den p =
    check_invariant
    @@
    if CCEqual.physical num Z.zero then zero
    else
      let p_num = Z.trailing_zeros num in
      let p_den = Z.trailing_zeros den in
      let p' = p_num + p - p_den in
      if Int.(abs p' < min_p) then
        if p' = 0 then
          reduce (Z.shift_right num p_num) (Z.shift_right den p_den) 0
        else if p' < 0 then
          reduce (Z.shift_right num p_num) (mul_2p den (-p_num - p)) 0
        else reduce (mul_2p num (p - p_den)) (Z.shift_right den p_den) 0
      else reduce (Z.shift_right num p_num) (Z.shift_right den p_den) p'

  let make_real num den p =
    let aux n d p =
      if CCEqual.physical d Z.one then { num = n; den = Z.one; p }
      else
        let num, den = reduce n d in
        { num; den; p }
    in
    reduce_p aux num den p

  let of_int i = make_real (Z.of_int i) Z.one 0
  let two = of_int 2

  let aors zaors x y =
    if CCEqual.physical x.den y.den then
      let p, x_num, y_num = norm_num x y in
      make_real (zaors x_num y_num) x.den p
    else
      let p, x_num, y_num = norm_num x y in
      make_real
        (zaors (Z.mul x_num y.den) (Z.mul y_num x.den))
        (Z.mul x.den y.den) p

  let add x y =
    let r = aors Z.add x y in
    (* let r' = Z_Q.add (to_q x) (to_q y) in
    assert (equal r (of_q r')); *)
    r

  let sub x y = aors Z.sub x y

  let mul x y =
    let x_num, y_den = reduce x.num y.den in
    let y_num, x_den = reduce y.num x.den in
    let aux num den p = { num; den; p } in
    reduce_p aux (Z.mul x_num y_num) (Z.mul x_den y_den) (x.p + y.p)

  let inv x =
    match Z.sign x.num with
    | 1 -> { num = x.den; den = x.num; p = -x.p }
    | -1 -> { num = Z.neg x.den; den = Z.neg x.num; p = -x.p }
    | _ -> invalid_arg "inv with zero"

  let div x y =
    assert (not (CCEqual.physical y.num Z.zero));
    if Z.sign y.num >= 0 then mul x { num = y.den; den = y.num; p = -y.p }
    else mul x { num = Z.neg y.den; den = Z.neg y.num; p = -y.p }

  let ge x y =
    let r = compare x y >= 0 in
    (* let r' = Z_Q.ge (to_q x) (to_q y) in
    assert (Bool.equal r r'); *)
    r

  let le x y =
    let r = compare x y <= 0 in
    (* let r' = Z_Q.le (to_q x) (to_q y) in
    assert (Bool.equal r r'); *)
    r

  let of_string_decimal x = of_q @@ Z_Q.of_string_decimal x
  let is_integer q = CCEqual.physical Z.one q.den && 0 <= q.p

  let is_unsigned_integer size q =
    is_integer q
    && Int.(Z.sign q.num >= 0)
    && Int.(q.p + Z.numbits q.num <= size)

  let unary_of f q = of_q (f (to_q q))
  let floor q = if is_integer q then q else unary_of Z_Q.floor q
  let ceil q = if is_integer q then q else unary_of Z_Q.ceil q
  let truncate q = if is_integer q then q else unary_of Z_Q.truncate q

  let make n d =
    let sd = Z.sign d in
    assert (sd <> 0);
    if sd > 0 then make_real n d 0 else make_real (Z.neg n) (Z.neg d) 0

  let binary_of f x y = of_q (f (to_q x) (to_q y))
  let div_e = binary_of Z_Q.div_e
  let div_t = binary_of Z_Q.div_t
  let div_f = binary_of Z_Q.div_f
  let mod_e = binary_of Z_Q.mod_e
  let mod_t = binary_of Z_Q.mod_t
  let mod_f = binary_of Z_Q.mod_f
  let sign x = Z.sign x.num
  let is_zero c = Int.equal (sign c) 0
  let none_zero c = if is_zero c then None else Some c
  let is_not_zero c = not (Int.equal (sign c) 0)
  let gen = QCheck2.Gen.map of_q Z_Q.gen

  let pow q n =
    if Int.(n < 0) then (
      let s = sign q in
      assert (Int.(s <> 0));
      if s < 0 then
        make_real
          (Z.pow (Z.neg q.den) Int.(-n))
          (Z.pow (Z.neg q.num) Int.(-n))
          (q.p * n)
      else make_real (Z.pow q.den Int.(-n)) (Z.pow q.num Int.(-n)) (q.p * n))
    else make_real (Z.pow q.num n) (Z.pow q.den n) (q.p * n)

  let of_string s = of_q @@ Z_Q.of_string s
  let to_string s = Z_Q.to_string (to_q s)
  let to_int q = Z_Q.to_int (to_q q)
  let to_z q = (to_q q).num
  let _ = to_z
  let _ = to_int
  let of_bigint q = of_q @@ Z_Q.of_bigint q
  let neg q = { num = Z.neg q.num; den = q.den; p = q.p }
  let abs q = { num = Z.abs q.num; den = q.den; p = q.p }
  let ( ~- ) = neg
  let ( ~+ ) x = x
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let mul_2exp x n = make_real x.num x.den Int.(x.p + n)
  let div_2exp x n = make_real x.num x.den Int.(x.p - n)
  let min a b = if compare a b <= 0 then a else b
  let max a b = if compare a b >= 0 then a else b
end

module Q = Z_Q

module A = struct
  let ctx = Calcium.CTX.mk ()

  module T = struct
    module A = struct
      include Calcium.CA

      let hash_fold_t s t = Base.Hash.fold_int s (hash ~ctx t)
    end

    type t = Q of Q.t | A of { a : A.t; size : Base.Int.t [@hash.ignore] }
    [@@deriving hash]

    let to_a = function Q q -> A.of_q ~ctx (Q.to_q q) | A a -> a.a

    let compare a b =
      match (a, b) with
      | Q a, Q b -> Q.compare a b
      | A a, Q b -> A.compare_q ~ctx a.a (Q.to_q b)
      | Q a, A b -> -A.compare_q ~ctx b.a (Q.to_q a)
      | A a, A b -> A.compare ~ctx a.a b.a

    let equal a b =
      match (a, b) with
      | Q a, Q b -> Q.equal a b
      | A a, Q b ->
          (not (Z.equal b.den Z.one)) && A.compare_q ~ctx a.a (Q.to_q b) = 0
      | Q a, A b ->
          (not (Z.equal a.den Z.one)) && A.compare_q ~ctx b.a (Q.to_q a) = 0
      | A a, A b -> A.compare ~ctx a.a b.a = 0

    let pp fmt = function
      | Q q -> Q.pp fmt q
      | A a -> Calcium.CA.pp ~ctx fmt a.a
  end

  include T
  include Colibri2_popop_lib.Popop_stdlib.MkDatatype (T)

  let zero = Q Q.zero
  let one = Q Q.one
  let minus_one = Q Q.minus_one
  let two = Q Q.two
  let sign = function Q q -> Q.sign q | A a -> A.sign ~ctx a.a
  let ge a b = compare a b >= 0
  let gt a b = compare a b > 0
  let le a b = compare a b <= 0
  let lt a b = compare a b < 0
  let min a b = if lt a b then a else b
  let max a b = if gt a b then a else b
  let of_string_decimal s = Q (Q.of_string_decimal s)
  let of_string s = Q (Q.of_string s)
  let to_string = function Q q -> Q.to_string q | A a -> A.to_string ~ctx a.a
  let is_integer = function Q q -> Q.is_integer q | A _ -> false
  (* by normalization *)

  let to_z = function Q q -> Q.to_z q | A _ -> assert false
  let to_int = function Q q -> Q.to_int q | A _ -> assert false

  let normalize (a : A.t) =
    match A.to_q ~ctx a with
    | None ->
        let size = String.length (A.to_string ~ctx a) in
        A { a; size }
    | Some q -> Q (Q.of_q q)

  let ( !! ) = normalize

  let is_unsigned_integer size = function
    | Q q -> Q.is_unsigned_integer size q
    | A _ -> false

  let of_q q = Q q
  let of_z z = Q (Q.of_bigint z)
  let of_int z = Q (Q.of_int z)
  let of_bigint = of_z
  let floor = function Q q -> Q (Q.floor q) | A a -> of_z (A.floor ~ctx a.a)
  let ceil = function Q q -> Q (Q.ceil q) | A a -> of_z (A.ceil ~ctx a.a)
  let prec = Z.shift_left Z.one 24

  let round_to_q ~prec round a =
    Q.make (round ~ctx (A.mul ~ctx (A.of_z ~ctx prec) a)) prec

  let floor_q ?(prec = prec) = function
    | Q q -> q
    | A a -> round_to_q ~prec A.floor a.a

  let ceil_q ?(prec = prec) = function
    | Q q -> q
    | A a -> round_to_q ~prec A.ceil a.a

  let truncate = function
    | Q q -> Q (Q.truncate q)
    | A a -> of_z (A.truncate ~ctx a.a)

  let neg = function Q q -> Q (Q.neg q) | A a -> normalize (A.neg ~ctx a.a)
  let inv = function Q q -> Q (Q.inv q) | A a -> normalize (A.inv ~ctx a.a)
  let abs = function Q q -> Q (Q.abs q) | A a -> normalize (A.abs ~ctx a.a)

  let combine2 name fq fa cv a b =
    match (a, b) with
    | Q a, Q b ->
        let r = fq a b in
        (* Debug.histo_incr stat_trailing (Z.trailing_zeros a.num);
        Debug.histo_incr stat_trailing (Z.trailing_zeros a.den); *)
        Q r
    | _ ->
        let r = cv (fa ~ctx (to_a a) (to_a b)) in
        Debug.dprintf7 debug "[A] %s %a %a is %a" name pp a pp b pp r;
        r

  (* todo special case for one, zero, ... *)
  let div = combine2 "div" Q.div A.div normalize
  let add = combine2 "add" Q.add A.add normalize
  let sub = combine2 "sub" Q.sub A.sub normalize
  let mul = combine2 "mul" Q.mul A.mul normalize
  let ( + ) = add
  let ( - ) = sub
  let ( ~- ) = neg
  let ( ~+ ) x = x
  let ( * ) = mul
  let ( / ) = div
  let div_e = combine2 "div_e" Q.div_e A.div_e of_z
  let div_t = combine2 "div_t" Q.div_t A.div_t of_z
  let div_f = combine2 "div_f" Q.div_f A.div_f of_z
  let mod_e = combine2 "mod_e" Q.mod_e A.mod_e normalize
  let mod_t = combine2 "mod_t" Q.mod_t A.mod_t normalize
  let mod_f = combine2 "mod_f" Q.mod_f A.mod_f normalize
  let is_zero c = match c with Q q -> Q.is_zero q | A _ -> false
  (* by normalization *)

  let round_down_to a m =
    match a with
    | Q q -> Q.round_down_to q m
    | A a ->
        let q' = A.floor ~ctx (A.div ~ctx a.a (A.of_q ~ctx (Q.to_q m))) in
        Q.mul (Q.of_bigint q') m

  let round_up_to a m =
    match a with
    | Q q -> Q.round_up_to q m
    | A a ->
        let q' = A.ceil ~ctx (A.div ~ctx a.a (A.of_q ~ctx (Q.to_q m))) in
        Q.mul (Q.of_bigint q') m

  let none_zero c = if is_zero c then None else Some c
  let is_not_zero c = not (Int.equal (sign c) 0)

  (* let a_gen : A.t QCheck.Gen.t = QChecl.Gen.empty
   *   let open QCheck.Gen in
   *   if false then
   *     let rec aux s =
   *       let coefs = small_list small_signed_int s in
   *       let u = Ocaml_poly.UPolynomial.construct (List.map Z.of_int coefs) in
   *       let roots = Ocaml_poly.UPolynomial.roots_isolate u in
   *       match roots with [] -> aux s | _ -> oneofl roots s
   *     in
   *     aux
   *   else fun s ->
   *     let a = (triple small_signed_int small_nat small_nat) s in
   *     let b = (triple small_signed_int small_nat small_nat) s in
   *     let coefs = [ a; b ] in
   *     List.fold_left
   *       (fun acc (p, c, r) ->
   *         A.add acc
   *           (A.mul (A.of_int p) (A.positive_root (A.of_int c) Int.(r + 1))))
   *       (A.zero ()) coefs *)

  let gen =
    let open QCheck2.Gen in
    frequency
      [ (999, map (fun q -> Q q) Q.gen) (* ; (1, map (fun a -> !!a) a_gen) *) ]

  let pow q n =
    match q with Q q -> Q (Q.pow q n) | A a -> !!(A.pow_int ~ctx a.a n)

  let positive_root q n =
    Debug.dprintf3 debug "[A] %i root computed: %a" n pp q;
    if n = 0 then one else !!(A.pow ~ctx (to_a q) (Z_Q.make Z.one (Z.of_int n)))

  let positive_pow q n =
    let r = !!(A.pow ~ctx (to_a q) (Q.to_q n)) in
    Debug.dprintf6 debug "[A] %a pow computed: %a is %a" Q.pp n pp q pp r;
    r

  let big_number = function
    | Q q -> Z.big_number q.num || Z.big_number q.den
    | A a -> Int.(a.size > Z.max_size * 4)

  let round_down_to_small_number x =
    if big_number x then
      match x with
      | Q q -> Q (Q.round_down_to_small_number q)
      | A a -> Q (Q.round_down_to_small_number (round_to_q ~prec A.floor a.a))
    else x

  let round_up_to_small_number x =
    if big_number x then
      match x with
      | Q q -> Q (Q.round_up_to_small_number q)
      | A a -> Q (Q.round_up_to_small_number (round_to_q ~prec A.ceil a.a))
    else x
end

(* let () = Ocaml_poly.PolyUtils.trace_enable "algebraic_number" *)

module Sequence = struct
  include Base.Sequence

  (** Fix interleave bad behavior *)
  let interleave s1 =
    let rec next (todo_stack, done_stack, s1) =
      match todo_stack with
      | s2 :: todo_stack -> (
          match Base.Sequence.Expert.next_step s2 with
          | Yield { value = x; state = s2 } ->
              Base.Sequence.Step.Yield
                { value = x; state = (todo_stack, s2 :: done_stack, s1) }
          | Skip { state = s2 } ->
              Skip { state = (todo_stack, s2 :: done_stack, s1) }
          | Done -> Skip { state = (todo_stack, done_stack, s1) })
      | [] -> (
          match (Base.Sequence.Expert.next_step s1, done_stack) with
          | Yield { value = t; state = s1 }, _ ->
              (* The original implementation added a skip here,
                 which delayed the yield. Many interleave one after another has a very bad complexity *)
              next (List.rev (t :: done_stack), [], s1)
          | Skip { state = s1 }, _ ->
              Skip { state = (List.rev done_stack, [], s1) }
          | Done, _ :: _ -> Skip { state = (List.rev done_stack, [], s1) }
          | Done, [] -> Done)
    in
    let state = ([], [], s1) in
    Base.Sequence.unfold_step ~init:state ~f:next

  let interleaved_cartesian_product s1 s2 =
    Base.Sequence.map s1 ~f:(fun x1 ->
        Base.Sequence.map s2 ~f:(fun x2 -> (x1, x2)))
    |> interleave

  let ( let+ ) x y = Base.Sequence.( >>| ) x y
  let ( let* ) t f = Base.Sequence.bind t ~f
  let ( and* ) x y = interleaved_cartesian_product x y
  let concat = interleave
end
