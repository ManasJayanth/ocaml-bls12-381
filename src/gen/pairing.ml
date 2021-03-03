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

module type RAW_STUBS = sig
  val pairing_check_1 : Bytes.t -> Bytes.t -> bool

  val pairing_check_2 : Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> bool

  val pairing_check_3 :
    Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> bool

  val pairing_check_4 :
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    bool

  val pairing_check_5 :
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    bool

  val pairing_check_6 :
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    bool
end

module Make
    (G1 : Elliptic_curve_sig.T)
    (G2 : Elliptic_curve_sig.T)
    (Stubs : RAW_STUBS) : sig
  val pairing_check : (G1.t * G2.t) list -> bool
end = struct
  let pairing_check xs =
    match xs with
    | [(g1, g2)] -> Stubs.pairing_check_1 (G1.to_bytes g1) (G2.to_bytes g2)
    | [(g1_1, g2_1); (g1_2, g2_2)] ->
        Stubs.pairing_check_2
          (G1.to_bytes g1_1)
          (G1.to_bytes g1_2)
          (G2.to_bytes g2_1)
          (G2.to_bytes g2_2)
    | [(g1_1, g2_1); (g1_2, g2_2); (g1_3, g2_3)] ->
        Stubs.pairing_check_3
          (G1.to_bytes g1_1)
          (G1.to_bytes g1_2)
          (G1.to_bytes g1_3)
          (G2.to_bytes g2_1)
          (G2.to_bytes g2_2)
          (G2.to_bytes g2_3)
    | [(g1_1, g2_1); (g1_2, g2_2); (g1_3, g2_3); (g1_4, g2_4)] ->
        Stubs.pairing_check_4
          (G1.to_bytes g1_1)
          (G1.to_bytes g1_2)
          (G1.to_bytes g1_3)
          (G1.to_bytes g1_4)
          (G2.to_bytes g2_1)
          (G2.to_bytes g2_2)
          (G2.to_bytes g2_3)
          (G2.to_bytes g2_4)
    | [(g1_1, g2_1); (g1_2, g2_2); (g1_3, g2_3); (g1_4, g2_4); (g1_5, g2_5)] ->
        Stubs.pairing_check_5
          (G1.to_bytes g1_1)
          (G1.to_bytes g1_2)
          (G1.to_bytes g1_3)
          (G1.to_bytes g1_4)
          (G1.to_bytes g1_5)
          (G2.to_bytes g2_1)
          (G2.to_bytes g2_2)
          (G2.to_bytes g2_3)
          (G2.to_bytes g2_4)
          (G2.to_bytes g2_5)
    | [ (g1_1, g2_1);
        (g1_2, g2_2);
        (g1_3, g2_3);
        (g1_4, g2_4);
        (g1_5, g2_5);
        (g1_6, g2_6) ] ->
        Stubs.pairing_check_6
          (G1.to_bytes g1_1)
          (G1.to_bytes g1_2)
          (G1.to_bytes g1_3)
          (G1.to_bytes g1_4)
          (G1.to_bytes g1_5)
          (G1.to_bytes g1_6)
          (G2.to_bytes g2_1)
          (G2.to_bytes g2_2)
          (G2.to_bytes g2_3)
          (G2.to_bytes g2_4)
          (G2.to_bytes g2_5)
          (G2.to_bytes g2_6)
    | _ -> assert false
end
