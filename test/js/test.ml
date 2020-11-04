(* let BLS12_381 = require("./bls12_381.js");
 * let a = BLS12_381.Fr.random();
 * let b = BLS12_381.Fr.random();
 * let a_times_b = BLS12_381.Fr.mul(a, b);
 * let b_times_a = BLS12_381.Fr.mul(b, a);
 * console.log(BLS12_381.Fr.eq(a_times_b, b_times_a)); *)

let () =
  print_endline Bls12_381.Fr.(to_string (mul (random ()) (random ())))
