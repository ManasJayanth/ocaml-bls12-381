let () =
  let dst = Bytes.of_string "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_" in
  let message = Bytes.of_string "Hello" in
  let res = Bls12_381.G1.hash_to_curve message dst in
  let res_bytes = Bls12_381.G1.to_bytes res in
  assert (Bls12_381.G1.check_bytes res_bytes) ;
  Printf.printf "%s\n" Hex.(show (of_bytes res_bytes))

let () =
  let dst = Bytes.of_string "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_" in
  let message = Bytes.of_string "Hello" in
  let res = Bls12_381.G2.hash_to_curve message dst in
  let res_bytes = Bls12_381.G2.to_bytes res in
  assert (Bls12_381.G2.check_bytes res_bytes) ;
  Printf.printf "%s\n" Hex.(show (of_bytes res_bytes))
