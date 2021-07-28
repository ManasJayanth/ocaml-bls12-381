let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done ;
    !lines
  with End_of_file ->
    close_in chan ;
    List.rev !lines

module type SIG_SCHEME = sig
  val name : string

  val sign : Bls12_381.Signature.sk -> Bytes.t -> Bytes.t

  val verify : Bls12_381.Signature.pk -> Bytes.t -> Bytes.t -> bool
end

module MakeProperties (Scheme : SIG_SCHEME) = struct
  let test_sign_and_verify_same_message_with_correct_keys () =
    let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
    let sk = Bls12_381.Signature.generate_sk ikm in
    let pk = Bls12_381.Signature.derive_pk sk in
    (* sign a random message *)
    let msg_length = 1 + Random.int 512 in
    let msg = Bytes.init msg_length (fun _i -> char_of_int (Random.int 256)) in
    let signature = Scheme.sign sk msg in
    assert (Scheme.verify pk msg signature)

  let test_sign_and_verify_different_message_with_correct_keys () =
    let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
    let sk = Bls12_381.Signature.generate_sk ikm in
    let pk = Bls12_381.Signature.derive_pk sk in
    (* sign a random message *)
    let msg_length = 1 + Random.int 512 in
    let msg = Bytes.init msg_length (fun _i -> char_of_int (Random.int 256)) in
    let msg'_length = 1 + Random.int 512 in
    let msg' =
      Bytes.init msg'_length (fun _i -> char_of_int (Random.int 256))
    in
    let signature = Scheme.sign sk msg in
    assert (not (Scheme.verify pk msg' signature))

  let test_sign_and_verify_different_message_with_different_keys () =
    let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
    let sk = Bls12_381.Signature.generate_sk ikm in
    let _pk = Bls12_381.Signature.derive_pk sk in

    let ikm' = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
    let sk' = Bls12_381.Signature.generate_sk ikm' in
    let pk' = Bls12_381.Signature.derive_pk sk' in

    (* sign a random message *)
    let msg_length = 1 + Random.int 512 in
    let msg = Bytes.init msg_length (fun _i -> char_of_int (Random.int 256)) in
    let msg'_length = 1 + Random.int 512 in
    let msg' =
      Bytes.init msg'_length (fun _i -> char_of_int (Random.int 256))
    in
    let signature = Scheme.sign sk msg in
    assert (not (Scheme.verify pk' msg' signature))

  let test_sign_and_verify_same_message_with_different_keys () =
    let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
    let sk = Bls12_381.Signature.generate_sk ikm in
    let _pk = Bls12_381.Signature.derive_pk sk in

    let ikm' = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
    let sk' = Bls12_381.Signature.generate_sk ikm' in
    let pk' = Bls12_381.Signature.derive_pk sk' in

    (* sign a random message *)
    let msg_length = 1 + Random.int 512 in
    let msg = Bytes.init msg_length (fun _i -> char_of_int (Random.int 256)) in
    let signature = Scheme.sign sk msg in
    assert (not (Scheme.verify pk' msg signature))

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf "Properties for %s" Scheme.name,
      [ test_case
          "Sign and verify same message with correct keys"
          `Quick
          (Test_ec_make.repeat
             100
             test_sign_and_verify_same_message_with_correct_keys);
        test_case
          "Sign and verify different message with correct keys"
          `Quick
          (Test_ec_make.repeat
             100
             test_sign_and_verify_same_message_with_correct_keys);
        test_case
          "Sign and verify same message with different keys"
          `Quick
          (Test_ec_make.repeat
             100
             test_sign_and_verify_same_message_with_different_keys);
        test_case
          "Sign and verify different message with different keys"
          `Quick
          (Test_ec_make.repeat
             100
             test_sign_and_verify_different_message_with_different_keys) ] )
end

module BasicProperties = MakeProperties (struct
  let name = "Basic"

  let sign = Bls12_381.Signature.Basic.sign

  let verify = Bls12_381.Signature.Basic.verify
end)

module AugProperties = MakeProperties (struct
  let name = "Message augmentation"

  let sign = Bls12_381.Signature.Aug.sign

  let verify = Bls12_381.Signature.Aug.verify
end)

let test_vectors_sig_g2_basic () =
  let vectors =
    [ ( "f2380acb0d869d1cf2e25a6bd46ebe49d1c9270624c5507be4299fe773749596d07d10f7c2be1c0b27e86f27b4a6f8dff68cfe5c0b4c58dad1b4ebec7bd00ab195fdd635d9fa8a15acf81816868d737b8922379648ed70022b98c388ede5355e4d50e6bc9ec57737d8843fabda78054e92777c4b90466a5af35dd79e5d7a81ce",
        "1fd3739f8a03a815199b4ca119cc46e267a65b317f2bfcf47b875e19ee44f675",
        "b6447d755280963752070e0743f6af17f5358c7a68ef2f2f14b56cc2c634e1980ea172d6ef9c9a9d0334cab0487e7e9c0f4bf1ac572639d5be2d9829284c2772550b8bbe3bde2a172ddabcf4ac521301da17f1f0db6996719d68a009ead2c480"
      );
      ( "c1328d8d2e5b6ffc850a9600bd6482518ddd9cee3fc9140febb72bcd444b0cd7e8074587d51b62cce4b3d4f34ad3355353fabe363369cf790db2df9fdac3a0ec4757e2dfb3b683eaa3e26531691ce765742e1c0bdc0e1028d347b6085fc459df0989c6a144271454eaffe413cae2ad7c8b2371fd2df1afffe56df727009765a2",
        "6e66891daee01bc967dcd58aa25c41113769dce35757886bad2c98b35b5302fb",
        "b40f67dad34672df7b112dfe5ad541654191d4d75c34f476129548b18f98c52811201185fff7b5ea19c993c1e32757d00f928be44310d10a9a0866d13b8571d4fee06972ee3f997795b1e7263e990846adf10b59384b3cb025b00c2bcc7f023f"
      ) ]
  in
  List.iter
    (fun (msg_str, sk_str, exp_signature_str) ->
      let msg = Hex.(to_bytes (`Hex msg_str)) in
      (* Sk is given in big endian, and sk_of_bytes_exn needs little endian *)
      let sk_be = Hex.(to_bytes (`Hex sk_str)) in
      let sk_le = Bytes.init 32 (fun i -> Bytes.get sk_be (32 - i - 1)) in
      let sk = Bls12_381.Signature.sk_of_bytes_exn @@ sk_le in
      (* Signature is given in compressed form. Computing the bytes for the
         uncompressed version
      *)
      let exp_signature = Hex.(to_bytes (`Hex exp_signature_str)) in
      let exp_signature = Bls12_381.G2.of_compressed_bytes_exn exp_signature in
      let exp_signature = Bls12_381.G2.to_bytes exp_signature in
      let signature = Bls12_381.Signature.Basic.sign sk msg in
      if not @@ Bytes.equal signature exp_signature then
        Alcotest.failf
          "Expected result is %s on input %s with sk %s, but computed %s"
          Hex.(show (Hex.of_bytes exp_signature))
          msg_str
          Hex.(show (of_bytes (Bls12_381.Signature.sk_to_bytes sk)))
          Hex.(show (Hex.of_bytes signature)))
    vectors

let test_vectors_sig_g2_basic_from_bls_sigs_ref_files () =
  let aux filename =
    let contents = read_file filename in
    List.iter
      (fun content ->
        let contents = String.split_on_char ' ' content in
        let (msg_str, ikm_str, expected_result_str) =
          (List.nth contents 0, List.nth contents 1, List.nth contents 2)
        in
        let msg = Hex.(to_bytes (`Hex msg_str)) in
        Printf.printf "Msg: %s\nikm: %s\n" msg_str ikm_str ;
        let ikm = Hex.to_bytes (`Hex ikm_str) in
        if Bytes.length ikm < 32 then ()
        else
          let sk = Bls12_381.Signature.generate_sk ikm in
          let sk_bytes = Bls12_381.Signature.sk_to_bytes sk in
          let sk_python =
            Bls12_381.Fr.of_string
              "24635582912660024530429349282963467630610468040467798128166931744376504592370"
          in
          Printf.printf
            "ikm: %s, Sk: 0x%s, sk python: %s\n"
            ikm_str
            Hex.(show (of_bytes sk_bytes))
            Hex.(show (of_bytes (Bls12_381.Fr.to_bytes sk_python))) ;
          let sk = Bls12_381.Signature.sk_of_bytes_exn sk_bytes in
          let expected_result = Hex.(to_bytes (`Hex expected_result_str)) in
          let expected_result =
            Bls12_381.G2.of_compressed_bytes_exn expected_result
          in
          let expected_result = Bls12_381.G2.(to_bytes expected_result) in
          let res = Bls12_381.Signature.Basic.sign sk msg in
          if not @@ Bytes.equal res expected_result then
            Alcotest.failf
              "Expected result is %s on input %s with ikm %s, but computed %s"
              Hex.(show (Hex.of_bytes expected_result))
              msg_str
              ikm_str
              Hex.(show (Hex.of_bytes res)))
      contents
  in
  aux "sig_g2_basic_fips_186_3_B233" ;
  aux "sig_g2_basic_fips_186_3_B283" ;
  aux "sig_g2_basic_fips_186_3_B409" ;
  aux "sig_g2_basic_fips_186_3_B571" ;
  aux "sig_g2_basic_fips_186_3_K233" ;
  aux "sig_g2_basic_fips_186_3_K409" ;
  aux "sig_g2_basic_fips_186_3_K571" ;
  aux "sig_g2_basic_fips_186_3_P224" ;
  aux "sig_g2_basic_fips_186_3_P256" ;
  aux "sig_g2_basic_fips_186_3_P384" ;
  aux "sig_g2_basic_fips_186_3_P521" ;
  aux "sig_g2_basic_rfc6979"

let () =
  let open Alcotest in
  run
    "BLS Signature"
    [ BasicProperties.get_tests ();
      AugProperties.get_tests ();
      ( "Test vectors from Bls_sigs_ref",
        [ test_case "Sign G2 basic" `Quick test_vectors_sig_g2_basic
          (* test_case
           *   "Sign G2 basic from file"
           *   `Quick
           *   test_vectors_sig_g2_basic_from_bls_sigs_ref_files *) ] ) ]
