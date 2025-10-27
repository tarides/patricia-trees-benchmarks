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

include module type of Std_sig

val nnil : 'a list -> bool

module Poly : sig
  type (_, _, _) t =
    | Eq : ('a, 'a, [< `Eq | `IsEq | `Ord ]) t
    | Neq : (_, _, [ `IsEq ]) t
    | Gt : (_, _, [ `Ord ]) t
    | Lt : (_, _, [ `Ord ]) t

  type ('a, 'b) eq = ('a, 'b, [ `Eq ]) t
  type ('a, 'b) iseq = ('a, 'b, [ `IsEq ]) t
  type ('a, 'b) ord = ('a, 'b, [ `Ord ]) t

  val iseq : ('a, 'b, [< `Eq | `IsEq | `Ord ]) t -> ('a, 'b) iseq

  exception NotEq

  val eq : ('a, 'b, [< `Eq | `IsEq | `Ord ]) t -> ('a, 'b) eq
end

module Goption : sig
  type (_, _) t = Some : 'a -> ('a, [ `Some ]) t | None : ('a, [ `None ]) t
end

module Z : sig
  include module type of Z with type t = Z.t
  include Colibri2_popop_lib.Popop_stdlib.Datatype with type t := t

  val is_zero : t -> bool
  val gen : t QCheck2.Gen.t
  val big_number : t -> bool
  val pp_hexa : t Fmt.t
end

module Z_Q : sig
  include module type of Q with type t = Q.t
  include Colibri2_popop_lib.Popop_stdlib.Datatype with type t := t

  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val inv : t -> t
  val abs : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sign : t -> int
  val zero : t
  val one : t
  val minus_one : t
  val two : t
  val make : Z.t -> Z.t -> t
  val of_bigint : Z.t -> t
  val mul_2exp : t -> int -> t
  val div_2exp : t -> int -> t
  val ge : t -> t -> bool
  val le : t -> t -> bool
  val of_string_decimal : string -> t
  val floor : t -> t
  val ceil : t -> t
  val truncate : t -> t
  val div_t : t -> t -> t
  val div_e : t -> t -> t
  val div_f : t -> t -> t
  val mod_t : t -> t -> t
  val mod_e : t -> t -> t
  val mod_f : t -> t -> t
  val pow : t -> int -> t
  val is_integer : t -> bool
  val ( ~- ) : t -> t
  val ( ~+ ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t

  val is_unsigned_integer : int -> t -> bool
  (** [is_unsigned_integer size q] checks that [q] is an integer that fits in
      [size] bits *)

  val none_zero : t -> t option
  (** return None if the input is zero otherwise Some of the value *)

  val is_zero : t -> bool
  val is_not_zero : t -> bool
  val gen : t QCheck2.Gen.t
  val to_q : t -> Q.t
  val of_q : Q.t -> t
end

module Q_P : sig
  type t

  include Colibri2_popop_lib.Popop_stdlib.Datatype with type t := t

  val to_q : t -> Q.t
  val of_q : Q.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val inv : t -> t
  val abs : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sign : t -> int
  val zero : t
  val one : t
  val minus_one : t
  val two : t
  val make : Z.t -> Z.t -> t
  val of_bigint : Z.t -> t
  val mul_2exp : t -> int -> t
  val div_2exp : t -> int -> t
  val ge : t -> t -> bool
  val le : t -> t -> bool
  val of_string_decimal : string -> t
  val floor : t -> t
  val ceil : t -> t
  val truncate : t -> t
  val div_t : t -> t -> t
  val div_e : t -> t -> t
  val div_f : t -> t -> t
  val mod_t : t -> t -> t
  val mod_e : t -> t -> t
  val mod_f : t -> t -> t
  val pow : t -> int -> t
  val is_integer : t -> bool
  val ( ~- ) : t -> t
  val ( ~+ ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t

  val is_unsigned_integer : int -> t -> bool
  (** [is_unsigned_integer size q] checks that [q] is an integer that fits in
      [size] bits *)

  val none_zero : t -> t option
  (** return None if the input is zero otherwise Some of the value *)

  val is_zero : t -> bool
  val is_not_zero : t -> bool
  val gen : t QCheck2.Gen.t
  val to_string : t -> string
  val of_string : string -> t
end

module Q : sig
  type t

  include Colibri2_popop_lib.Popop_stdlib.Datatype with type t := t

  val to_q : t -> Q.t
  val of_q : Q.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val inv : t -> t
  val abs : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sign : t -> int
  val zero : t
  val one : t
  val minus_one : t
  val two : t
  val make : Z.t -> Z.t -> t
  val of_bigint : Z.t -> t
  val mul_2exp : t -> int -> t
  val div_2exp : t -> int -> t
  val ge : t -> t -> bool
  val le : t -> t -> bool
  val of_string_decimal : string -> t
  val floor : t -> t
  val ceil : t -> t
  val truncate : t -> t
  val div_t : t -> t -> t
  val div_e : t -> t -> t
  val div_f : t -> t -> t
  val mod_t : t -> t -> t
  val mod_e : t -> t -> t
  val mod_f : t -> t -> t
  val pow : t -> int -> t
  val is_integer : t -> bool
  val ( ~- ) : t -> t
  val ( ~+ ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t

  val is_unsigned_integer : int -> t -> bool
  (** [is_unsigned_integer size q] checks that [q] is an integer that fits in
      [size] bits *)

  val none_zero : t -> t option
  (** return None if the input is zero otherwise Some of the value *)

  val is_zero : t -> bool
  val is_not_zero : t -> bool
  val gen : t QCheck2.Gen.t
  val gcd : t -> t -> t
  val gcdext : t -> t -> t * Z.t * Z.t
  val lcm : t -> t -> t
  val divisible : t -> t -> bool
  val divexact : t -> t -> Z.t

  val round_down_to : t -> t -> t
  (** [round_down_to a m] find the biggest number divisible by m and smaller or
      equal to a *)

  val round_up_to : t -> t -> t
  (** [round_up_to a m] find the smallest number divisible by m and bigger or
      equal to a *)

  (** {2 Numerators and denominators can grow fast}*)

  val big_number : t -> bool

  val round_down_to_small_number : t -> t
  (** Give a smaller number *)

  val round_up_to_small_number : t -> t
  (** Give a bigger number *)
end

(** Algebraic number with use of rational when possible *)
module A : sig
  (** {!A} allows to determine certainly if something is an integer, but is not
      complete for rational. So Q is always used for integer and as best effort
      for rational *)
  type t = private Q of Q.t | A of { a : Calcium.CA.t; size : int }

  val zero : t
  val one : t

  val minus_one : t
  (** 0, 1, -1. *)

  val of_bigint : Z.t -> t
  val of_z : Z.t -> t
  val of_int : int -> t
  val to_z : t -> Z.t

  val to_int : t -> int
  (** suppose that it is an integer *)

  val of_q : Q.t -> t
  val sign : t -> int

  include Colibri2_popop_lib.Popop_stdlib.Datatype with type t := t

  val to_string : t -> string
  val two : t
  val ge : t -> t -> bool
  val le : t -> t -> bool
  val gt : t -> t -> bool
  val lt : t -> t -> bool

  val of_string : string -> t
  (** integer *)

  val of_string_decimal : string -> t
  val floor : t -> t
  val ceil : t -> t
  val truncate : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val inv : t -> t
  val abs : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val div_t : t -> t -> t
  val div_e : t -> t -> t
  val div_f : t -> t -> t
  val mod_t : t -> t -> t
  val mod_e : t -> t -> t
  val mod_f : t -> t -> t
  val pow : t -> int -> t
  val is_integer : t -> bool

  val is_unsigned_integer : int -> t -> bool
  (** [is_unsigned_integer size q] checks that [q] is an integer that fits in
      [size] bits *)

  val none_zero : t -> t option
  (** return None if the input is zero otherwise Some of the value *)

  val is_zero : t -> bool
  val is_not_zero : t -> bool
  val gen : t QCheck2.Gen.t
  val ( ~- ) : t -> t
  val ( ~+ ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val positive_root : t -> int -> t

  val positive_pow : t -> Q.t -> t
  (** the numerator and denominator should fit in an int *)

  val floor_q : ?prec:Z.t -> t -> Q.t
  (** approximation by a smaller rationnal, prec how many bits in the fraction
      part *)

  val ceil_q : ?prec:Z.t -> t -> Q.t
  (** approximation by a bigger rationnal *)

  val round_down_to : t -> Q.t -> Q.t
  (** [round_down_to a m] find the biggest number divisible by m and smaller or
      equal to a *)

  val round_up_to : t -> Q.t -> Q.t
  (** [round_up_to a m] find the smallest number divisible by m and bigger or
      equal to a *)

  val ctx : Calcium.CTX.t

  val big_number : t -> bool
  (** numbers are too big, slow convergence *)

  val round_down_to_small_number : t -> t
  (** Give a smaller number *)

  val round_up_to_small_number : t -> t
  (** Give a bigger number *)
end

module Sequence : sig
  include module type of Base.Sequence

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val concat : 'a t t -> 'a t
end
