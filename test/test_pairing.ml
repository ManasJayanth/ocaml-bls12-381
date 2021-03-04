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

open Bls12_381

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module Properties = struct
  let with_zero_as_first_component () =
    assert (Pairing.pairing_check [(G1.zero, G2.random ())])

  let with_zero_as_second_component () =
    assert (Pairing.pairing_check [(G1.random (), G2.zero)])

  let test_with_one_inverse () =
    let g1 = G1.random () in
    let g2 = G2.random () in
    assert (Pairing.pairing_check [(g1, g2); (G1.negate g1, g2)]) ;
    assert (Pairing.pairing_check [(g1, g2); (g1, G2.negate g2)])

  let rec test_random_points () =
    let n = Random.int 6 in
    if n <= 0 then test_random_points ()
    else
      let points = List.init n (fun _ -> (G1.random (), G2.random ())) in
      assert (not (Pairing.pairing_check points))
end

let test_vectors_one_one () =
  assert (not (Pairing.pairing_check [(G1.one, G2.one)]))

let () =
  let open Alcotest in
  run
    "Pairing"
    [ ( "Properties",
        [ test_case
            "with zero as first component"
            `Quick
            (repeat 100 Properties.with_zero_as_first_component);
          test_case
            "with zero as second component"
            `Quick
            (repeat 100 Properties.with_zero_as_second_component);
          test_case
            "with one inverse"
            `Quick
            (repeat 100 Properties.test_with_one_inverse);
          test_case
            "random points has low probability to be evaluated to one"
            `Quick
            (repeat 10 Properties.test_random_points);
          test_case
            "test vectors pairing of one and one"
            `Quick
            (repeat 1 test_vectors_one_one) ] ) ]
