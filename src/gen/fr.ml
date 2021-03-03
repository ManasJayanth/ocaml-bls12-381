(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type RAW = sig
  val order : Z.t

  val size_in_bytes : int

  val check_bytes : Bytes.t -> bool

  val is_zero : Bytes.t -> bool

  val is_one : Bytes.t -> bool

  val random : unit -> Bytes.t

  val zero : unit -> Bytes.t

  val one : unit -> Bytes.t

  val add : Bytes.t -> Bytes.t -> Bytes.t

  val mul : Bytes.t -> Bytes.t -> Bytes.t

  val unsafe_inverse : Bytes.t -> Bytes.t

  val eq : Bytes.t -> Bytes.t -> bool

  val negate : Bytes.t -> Bytes.t

  val square : Bytes.t -> Bytes.t

  val double : Bytes.t -> Bytes.t

  val pow : Bytes.t -> Bytes.t -> Bytes.t
end

module type T = sig
  include Ff_sig.PRIME

  (** Check if a point, represented as a byte array, is in the field **)
  val check_bytes : Bytes.t -> bool
end

module MakeFr (Stubs : RAW) : T = struct
  type t = Bytes.t

  exception Not_in_field of Bytes.t

  let order = Stubs.order

  let size_in_bytes = Stubs.size_in_bytes

  let pad_if_require bs =
    if Bytes.length bs < size_in_bytes then (
      let padded_bytes = Bytes.make size_in_bytes '\000' in
      Bytes.blit bs 0 padded_bytes 0 (Bytes.length bs) ;
      padded_bytes )
    else bs

  let check_bytes bs =
    if Bytes.length bs = size_in_bytes then Stubs.check_bytes bs else false

  let of_bytes_opt bs =
    let bs = pad_if_require bs in
    if check_bytes bs then Some bs else None

  let of_bytes_exn (g : Bytes.t) : t =
    let g = pad_if_require g in
    if check_bytes g then g else raise (Not_in_field g)

  let to_bytes g = g

  let is_zero g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    Stubs.is_zero g

  let is_one g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    Stubs.is_one g

  let zero =
    let g = Stubs.zero () in
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    g

  let one =
    let g = Stubs.one () in
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    g

  let random ?state () =
    ignore state ;
    let g = Stubs.random () in
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    g

  let rec non_null_random ?state () =
    ignore state ;
    let r = random () in
    if is_zero r then non_null_random () else r

  let add x y =
    assert (Bytes.length x = Stubs.size_in_bytes) ;
    assert (Bytes.length y = Stubs.size_in_bytes) ;
    let res = Stubs.add x y in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let ( + ) = add

  let mul x y =
    assert (Bytes.length x = Stubs.size_in_bytes) ;
    assert (Bytes.length y = Stubs.size_in_bytes) ;
    let res = Stubs.mul x y in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let ( * ) = mul

  let inverse_exn g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.unsafe_inverse g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let inverse_opt g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    if is_zero g then None
    else
      let res = Stubs.unsafe_inverse g in
      assert (Bytes.length res = Stubs.size_in_bytes) ;
      Some res

  let negate g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.negate g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let ( - ) = negate

  let sub a b = add a (negate b)

  let square g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.square g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let double g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.double g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let eq x y = Stubs.eq x y

  let ( = ) = eq

  let pow x n =
    let n = Z.erem n (Z.pred Stubs.order) in
    (* sign is removed by to_bits, but that's fine because we used mod before *)
    let n = Bytes.of_string (Z.to_bits n) in
    let bytes_size_n = Bytes.length n in
    let padded_n =
      Bytes.init Stubs.size_in_bytes (fun i ->
          if i < bytes_size_n then Bytes.get n i else char_of_int 0)
    in
    let res = Stubs.pow (to_bytes x) padded_n in
    (* assert (Bytes.length res = Stubs.size_in_bytes) ; *)
    res

  let ( ** ) = pow

  let div_exn a b =
    if b = zero then raise Division_by_zero else mul a (inverse_exn b)

  let div_opt a b = if b = zero then None else Some (mul a (inverse_exn b))

  let ( / ) = div_exn

  let two_z = Z.succ Z.one

  let factor_power_of_two =
    let rec aux i n =
      let (q, r) = Z.ediv_rem n two_z in
      if Z.equal r Z.zero then aux Int.(succ i) q else (i, n)
    in
    aux 0 (Z.pred order)

  let empty () = Bytes.make size_in_bytes '\000'

  let to_string a = Z.to_string (Z.of_bits (Bytes.to_string (to_bytes a)))

  let to_z a = Z.of_bits (Bytes.to_string (to_bytes a))

  let of_string s =
    let g = empty () in
    let s = Bytes.of_string (Z.to_bits (Z.erem (Z.of_string s) order)) in
    Bytes.blit s 0 g 0 (min (Bytes.length s) size_in_bytes) ;
    of_bytes_exn g

  let of_z z =
    let z = Bytes.of_string (Z.to_bits (Z.erem z order)) in
    let x = empty () in
    Bytes.blit z 0 x 0 (min (Bytes.length z) size_in_bytes) ;
    of_bytes_exn x

  let legendre_symbol x =
    if is_zero x then Z.zero
    else if is_one (pow x (Z.divexact (Z.pred order) (Z.of_int 2))) then Z.one
    else Z.neg Z.one

  let is_quadratic_residue x =
    if is_zero x then true else Z.equal (legendre_symbol x) Z.one

  let rec pick_non_square () =
    let z = random () in
    if Z.equal (legendre_symbol z) (Z.of_int (-1)) then z
    else pick_non_square ()

  let sqrt_opt x =
    if not (is_quadratic_residue x) then None
    else
      (* https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm *)
      let (s, q) = factor_power_of_two in
      (* implies p = 3 mod 4 *)
      if Int.equal s 1 then
        (* r = x^((p + 1) / 4) *)
        let r = pow x (Z.divexact (Z.succ order) (Z.of_string "4")) in
        Some r
      else
        let rec compute_lowest_n_2th_root_of_unity (i : int) x upper : int =
          let x = square x in
          if is_one x then i
          else if Int.(equal i upper) then
            failwith "Upperbound should be higher"
            (* should never happen in this case, just being explicit *)
          else compute_lowest_n_2th_root_of_unity (Int.succ i) x upper
        in
        let z = pick_non_square () in
        let c = pow z q in
        let rec aux m c t r =
          if eq t zero then zero (* case x is zero *)
          else if eq t one then r (* base case *)
          else
            let i = compute_lowest_n_2th_root_of_unity 1 t m in
            let b = pow c (Z.pow two_z Int.(pred (sub m i))) in
            let m = i in
            let c = mul b b in
            let t = mul t c in
            let r = mul r b in
            aux m c t r
        in
        Some (aux s c (pow x q) (pow x (Z.divexact (Z.succ q) two_z)))
end
