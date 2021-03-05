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

open Ctypes

module MakeFieldBindings (S : sig
  val field_name : string
end)
(F : Cstubs.FOREIGN) =
struct
  open F

  let check_bytes =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_check_bytes" S.field_name)
      (ocaml_bytes @-> returning bool)

  let is_zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_is_zero" S.field_name)
      (ocaml_bytes @-> returning bool)

  let is_one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_is_one" S.field_name)
      (ocaml_bytes @-> returning bool)

  let random =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_random" S.field_name)
      (ocaml_bytes @-> returning void)

  let zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_zero" S.field_name)
      (ocaml_bytes @-> returning void)

  let one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_one" S.field_name)
      (ocaml_bytes @-> returning void)

  let add =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_add" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let mul =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_mul" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let unsafe_inverse =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_unsafe_inverse" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let eq =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_eq" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let negate =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_negate" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let square =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_square" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let double =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_double" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let pow =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_pow" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)
end

module Fr = MakeFieldBindings (struct
  let field_name = "fr"
end)

module MakeGroupBindings (S : sig
  val group_name : string
end)
(F : Cstubs.FOREIGN) =
struct
  open F

  let check_bytes =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_check_bytes" S.group_name)
      (ocaml_bytes @-> returning bool)

  let one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_one" S.group_name)
      (ocaml_bytes @-> returning void)

  let zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_zero" S.group_name)
      (ocaml_bytes @-> returning void)

  let random =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_random" S.group_name)
      (ocaml_bytes @-> returning void)

  let add =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_add" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let double =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_double" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let negate =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_negate" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let eq =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_eq" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let is_zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_is_zero" S.group_name)
      (ocaml_bytes @-> returning bool)

  let mul =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_mul" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)
end

module G1 (F : Cstubs.FOREIGN) = struct
  include MakeGroupBindings
            (struct
              let group_name = "g1"
            end)
            (F)
end

module G2 (F : Cstubs.FOREIGN) = struct
  include MakeGroupBindings
            (struct
              let group_name = "g2"
            end)
            (F)
end

module Pairing (F : Cstubs.FOREIGN) = struct
  open F

  let pairing_check_1 =
    foreign
      "rustc_bls12_381_pairing_check_1"
      (ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let pairing_check_2 =
    foreign
      "rustc_bls12_381_pairing_check_2"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> returning bool )

  let pairing_check_3 =
    foreign
      "rustc_bls12_381_pairing_check_3"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> returning bool )

  let pairing_check_4 =
    foreign
      "rustc_bls12_381_pairing_check_4"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> returning bool )

  let pairing_check_5 =
    foreign
      "rustc_bls12_381_pairing_check_5"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> returning bool )

  let pairing_check_6 =
    foreign
      "rustc_bls12_381_pairing_check_6"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> returning bool )
end
