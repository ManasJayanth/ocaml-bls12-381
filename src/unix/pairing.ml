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

module Pairing_stubs = Rustc_bls12_381_bindings.Pairing (Rustc_bls12_381_stubs)

(* The pairing goes in a finite field, not a group. We strengthen the signature *)
module Raw_Stubs = struct
  let pairing_check_1 g1 g2 =
    Pairing_stubs.pairing_check_1
      (Ctypes.ocaml_bytes_start g1)
      (Ctypes.ocaml_bytes_start g2)

  let pairing_check_2 g1_1 g1_2 g2_1 g2_2 =
    Pairing_stubs.pairing_check_2
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)

  let pairing_check_3 g1_1 g1_2 g1_3 g2_1 g2_2 g2_3 =
    Pairing_stubs.pairing_check_3
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)

  let pairing_check_4 g1_1 g1_2 g1_3 g1_4 g2_1 g2_2 g2_3 g2_4 =
    Pairing_stubs.pairing_check_4
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g1_4)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)
      (Ctypes.ocaml_bytes_start g2_4)

  let pairing_check_5 g1_1 g1_2 g1_3 g1_4 g1_5 g2_1 g2_2 g2_3 g2_4 g2_5 =
    Pairing_stubs.pairing_check_5
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g1_4)
      (Ctypes.ocaml_bytes_start g1_5)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)
      (Ctypes.ocaml_bytes_start g2_4)
      (Ctypes.ocaml_bytes_start g2_5)

  let pairing_check_6 g1_1 g1_2 g1_3 g1_4 g1_5 g1_6 g2_1 g2_2 g2_3 g2_4 g2_5
      g2_6 =
    Pairing_stubs.pairing_check_6
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g1_4)
      (Ctypes.ocaml_bytes_start g1_5)
      (Ctypes.ocaml_bytes_start g1_6)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)
      (Ctypes.ocaml_bytes_start g2_4)
      (Ctypes.ocaml_bytes_start g2_5)
      (Ctypes.ocaml_bytes_start g2_6)
end

include Bls12_381_gen.Pairing.Make (G1) (G2) (Raw_Stubs)
