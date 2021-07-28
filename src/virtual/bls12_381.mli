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

module Fq12 : Bls12_381_gen.Fq12.T

module Fr : Bls12_381_gen.Fr.T

module G1 : Bls12_381_gen.G1.T with type Scalar.t = Fr.t

module G2 : Bls12_381_gen.G2.T with type Scalar.t = Fr.t

module Pairing : sig
  exception FailToComputeFinalExponentiation of Fq12.t

  val miller_loop : (G1.t * G2.t) list -> Fq12.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.t -> G2.t -> Fq12.t

  (** Compute a pairing result of a list of points *)
  val pairing : G1.t -> G2.t -> Fq12.t

  (** Compute the final exponentiation of the given point. Returns a [None] if
        the point is null *)
  val final_exponentiation_opt : Fq12.t -> Fq12.t option

  (** Compute the final exponentiation of the given point. Raise
        [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : Fq12.t -> Fq12.t
end

module Signature : sig
  type sk

  type pk

  val sk_of_bytes_exn : Bytes.t -> sk

  val sk_to_bytes : sk -> Bytes.t

  val generate_sk : ?key_info:Bytes.t -> Bytes.t -> sk

  val derive_pk : sk -> pk

  (**
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.1

    In a basic scheme, rogue key attacks are handled by requiring all
    messages signed by an aggregate signature to be distinct.  This
    requirement is enforced in the definition of AggregateVerify.

    The Sign and Verify functions are identical to CoreSign and
    CoreVerify (Section 2), respectively.
  *)
  module Basic : sig
    val sign : sk -> Bytes.t -> Bytes.t

    val verify : pk -> Bytes.t -> Bytes.t -> bool
  end

  (**
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2

    In a message augmentation scheme, signatures are generated over the
    concatenation of the public key and the message, ensuring that
    messages signed by different public keys are distinct.
  *)
  module Aug : sig
    val sign : sk -> Bytes.t -> Bytes.t

    val verify : pk -> Bytes.t -> Bytes.t -> bool
  end

  (* module Pop : sig
   *   val sign : sk -> Bytes.t -> Bytes.t
   * 
   *   val verify : pk -> Bytes.t -> Bytes.t -> bool
   * end *)
end
