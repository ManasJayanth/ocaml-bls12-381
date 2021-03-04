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

module ValueGeneration = Test_ec_make.MakeValueGeneration (Bls12_381.G2)
module IsZero = Test_ec_make.MakeIsZero (Bls12_381.G2)
module Equality = Test_ec_make.MakeEquality (Bls12_381.G2)
module ECProperties = Test_ec_make.MakeECProperties (Bls12_381.G2)

module Representation = struct
  let test_zero_has_first_byte_at_64 () =
    assert (int_of_char (Bytes.get Bls12_381.G2.(to_bytes zero) 0) = 64)

  let test_random_has_first_byte_strictly_lower_than_64 () =
    assert (int_of_char (Bytes.get Bls12_381.G2.(to_bytes (random ())) 0) < 64)

  let get_tests () =
    let open Alcotest in
    ( "Representation of G2",
      [ test_case
          "zero has first byte at 64"
          `Quick
          test_zero_has_first_byte_at_64;
        test_case
          "random has first byte strictly lower than 64"
          `Quick
          (Test_ec_make.repeat
             1000
             test_random_has_first_byte_strictly_lower_than_64) ] )
end

let () =
  let open Alcotest in
  run
    "G2"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      Representation.get_tests ();
      ECProperties.get_tests () ]
