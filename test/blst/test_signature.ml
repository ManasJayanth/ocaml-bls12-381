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
  val sign : Bls12_381.Signature.sk -> Bytes.t -> Bytes.t

  val verify : Bls12_381.Signature.pk -> Bytes.t -> Bytes.t -> bool
end

module MakeProperties (Scheme : sig
  include SIG_SCHEME

  val name : string
end) =
struct
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

  let test_verify_signature_which_represents_point_on_the_curve_but_not_in_the_prime_subgroup
      () =
    (* These points are on the curve, but not in the subgroup, see test_g2.ml to
       see how it has been generated. It is given in the compressed form as required for the signature.
    *)
    let test_vectors =
      [ "a7da246233ad216e60ee03070a0916154ae9f9dc23310c1191dfb4e277fc757f58a5cf5bdf7a9f322775143c37539cb90798205fd56217b682d5656f7ac7bc0da111dee59d3f863f1b040be659eda7941afb9f1bc5d0fe2beb5e2385e2cfe9ee";
        "b112717bbcd089ea99e8216eab455ea5cd462b0b3e3530303b83477f8e1bb7abca269fec10b3eb998f7f6fd1799d58ff11ed0a53bf75f91d2bf73d11bd52d061f401ac6a6ec0ef4a163e480bac85e75b97cb556f500057b9ef4b28bfe196791d";
        "86e5fa411047d9632c95747bea64d973757904c935ac0741b9eeefa2c7c4e439baf1d2c1e8633ba6c884ed9fdf1ffbdd129a32c046f355c5126254973115d6df32904498db6ca959d5bf1869f235be4c0e60fc334ed493f864476907cadfef2c";
        "88c83e90520a5ea31733cc01e3589e10b2ed755e2faade29199f97645fbf73f52b29297c22a3b1c4fcd3379bceeec832091df6fb3b9d23f04e8267fc41e578002484155562e70f488c2a4c6b11522c66736bc977755c257478f3022656abb630";
        "a25099811f52ad463c762197466c476a03951afdb3f0a457efa2b9475376652fba7b2d56f3184dad540a234d471c53a113203f73dd661694586c75d9c418d34cd16504356253e3ba4618f61cbee02880a43efeacb8f9fe1fdc84ceec4f780ba2";
        "990f5e1d200d1b9ab842c516ce50992730917a8b2e95ee1a4b830d7d9507c6846ace7a0eed8831a8d1f1e233cd24581215fe8fe85a99f4ca3fe046dba8ac6377fc3c10d73fa94b25c2d534d7a587a507b498754a2534cd85777b2a7f2978eec6";
        "a29415562a1d18b11ec8ab2e0b347a9417f9e904cf25f9b1dc40f235507814371fb4568cc1070a0b8c7baf39e0039d1e0b49d4352b095883ccc262e23d8651c49c39c06d0a920d40b2765d550a78c4c1940c8a2b6843a0063402c169f079f0ae";
        "8a257ed6d95cb226c3eb57218bd075ba27164fc1b972c4230ee70c7b81c89d38253ccf7ed2896aa5eb3d9fd6021fac000e368080e705f2a65c919539e2d28e6dd1117296b4210fd56db8d96891f8586bd333e9c47f838ed436659a1dafaee16c"
      ]
    in
    List.iter
      (fun signature_hex ->
        let ikm = Bytes.init 32 (fun _i -> char_of_int @@ Random.int 256) in
        let sk = Bls12_381.Signature.generate_sk ikm in
        let pk = Bls12_381.Signature.derive_pk sk in
        let msg = Bytes.of_string "Hello" in
        let signature = Hex.(to_bytes (`Hex signature_hex)) in
        assert (not (Scheme.verify pk msg signature)))
      test_vectors

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
          "verify a random message with a signature in the prime subgroup"
          `Quick
          test_verify_signature_which_represents_point_on_the_curve_but_not_in_the_prime_subgroup;
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

module PopProperties = MakeProperties (struct
  let name = "Proof of possession"

  let sign = Bls12_381.Signature.Pop.sign

  let verify = Bls12_381.Signature.Pop.verify
end)

module MakeTestVectorsFromFile (Scheme : SIG_SCHEME) = struct
  let test_vectors_from_bls_sigs_ref_files files () =
    let aux filename =
      let contents = read_file filename in
      List.iter
        (fun content ->
          let contents = String.split_on_char ' ' content in
          let (msg_str, ikm_str, expected_result_str) =
            (List.nth contents 0, List.nth contents 1, List.nth contents 2)
          in
          let msg = Hex.(to_bytes (`Hex msg_str)) in
          let ikm = Hex.to_bytes (`Hex ikm_str) in
          if Bytes.length ikm < 32 then ()
          else
            let sk = Bls12_381.Signature.generate_sk ikm in
            let expected_result = Hex.(to_bytes (`Hex expected_result_str)) in
            let res = Scheme.sign sk msg in
            if not @@ Bytes.equal res expected_result then
              Alcotest.failf
                "Expected result is %s on input %s with ikm %s, but computed %s"
                Hex.(show (Hex.of_bytes expected_result))
                msg_str
                ikm_str
                Hex.(show (Hex.of_bytes res)))
        contents
    in
    List.iter (fun filename -> aux filename) files
end

let sig_g2_basic_files =
  [ "sig_g2_basic_fips_186_3_B233_blst";
    "sig_g2_basic_fips_186_3_B283_blst";
    "sig_g2_basic_fips_186_3_B409_blst";
    "sig_g2_basic_fips_186_3_B571_blst";
    "sig_g2_basic_fips_186_3_K233_blst";
    "sig_g2_basic_fips_186_3_K409_blst";
    "sig_g2_basic_fips_186_3_K571_blst";
    "sig_g2_basic_fips_186_3_P224_blst";
    "sig_g2_basic_fips_186_3_P256_blst";
    "sig_g2_basic_fips_186_3_P384_blst";
    "sig_g2_basic_fips_186_3_P521_blst";
    "sig_g2_basic_rfc6979_blst" ]

module TestVectorsSigG2Basic =
  MakeTestVectorsFromFile (Bls12_381.Signature.Basic)

let sig_g2_aug_files =
  [ "sig_g2_aug_fips_186_3_B233_blst";
    "sig_g2_aug_fips_186_3_B283_blst";
    "sig_g2_aug_fips_186_3_B409_blst";
    "sig_g2_aug_fips_186_3_B571_blst";
    "sig_g2_aug_fips_186_3_K233_blst";
    "sig_g2_aug_fips_186_3_K409_blst";
    "sig_g2_aug_fips_186_3_K571_blst";
    "sig_g2_aug_fips_186_3_P224_blst";
    "sig_g2_aug_fips_186_3_P256_blst";
    "sig_g2_aug_fips_186_3_P384_blst";
    "sig_g2_aug_fips_186_3_P521_blst";
    "sig_g2_aug_rfc6979_blst" ]

module TestVectorsSigG2Aug = MakeTestVectorsFromFile (Bls12_381.Signature.Aug)

let sig_g2_pop_files =
  [ "sig_g2_pop_fips_186_3_B233_blst";
    "sig_g2_pop_fips_186_3_B283_blst";
    "sig_g2_pop_fips_186_3_B409_blst";
    "sig_g2_pop_fips_186_3_B571_blst";
    "sig_g2_pop_fips_186_3_K233_blst";
    "sig_g2_pop_fips_186_3_K409_blst";
    "sig_g2_pop_fips_186_3_K571_blst";
    "sig_g2_pop_fips_186_3_P224_blst";
    "sig_g2_pop_fips_186_3_P256_blst";
    "sig_g2_pop_fips_186_3_P384_blst";
    "sig_g2_pop_fips_186_3_P521_blst";
    "sig_g2_pop_rfc6979_blst" ]

module TestVectorsSigG2Pop = MakeTestVectorsFromFile (Bls12_381.Signature.Pop)

let pop_g2_files =
  [ "pop_g2_fips_186_3_B233_blst";
    "pop_g2_fips_186_3_B283_blst";
    "pop_g2_fips_186_3_B409_blst";
    "pop_g2_fips_186_3_B571_blst";
    "pop_g2_fips_186_3_K233_blst";
    "pop_g2_fips_186_3_K409_blst";
    "pop_g2_fips_186_3_K571_blst";
    "pop_g2_fips_186_3_P224_blst";
    "pop_g2_fips_186_3_P256_blst";
    "pop_g2_fips_186_3_P384_blst";
    "pop_g2_fips_186_3_P521_blst";
    "pop_g2_rfc6979_blst" ]

let test_pop_g2_from_blst_sigs_ref_files files () =
  let aux filename =
    let contents = read_file filename in
    List.iter
      (fun content ->
        let contents = String.split_on_char ' ' content in
        let (ikm_str, exp_result_str) =
          (List.nth contents 1, List.nth contents 2)
        in
        let ikm_bytes = Hex.(to_bytes (`Hex ikm_str)) in
        if Bytes.length ikm_bytes < 32 then ()
        else
          let sk = Bls12_381.Signature.generate_sk ikm_bytes in
          let exp_result_bytes = Hex.(to_bytes (`Hex exp_result_str)) in
          let res = Bls12_381.Signature.Pop.pop_prove sk in
          if not @@ Bytes.equal res exp_result_bytes then
            Alcotest.failf
              "Expected result is %s with ikm %s, but computed %s"
              exp_result_str
              ikm_str
              Hex.(show (Hex.of_bytes res)))
      contents
  in
  List.iter (fun filename -> aux filename) files

let test_pop_prove_verify_with_correct_keys () =
  let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
  let sk = Bls12_381.Signature.generate_sk ikm in
  let pk = Bls12_381.Signature.derive_pk sk in
  (* sign a random message *)
  let proof = Bls12_381.Signature.Pop.pop_prove sk in
  assert (Bls12_381.Signature.Pop.pop_verify pk proof)

let test_pop_prove_verify_with_different_pk_for_verify () =
  let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
  let ikm' = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
  let sk = Bls12_381.Signature.generate_sk ikm in
  let pk' = Bls12_381.Signature.(derive_pk (generate_sk ikm')) in
  (* sign a random message *)
  let proof = Bls12_381.Signature.Pop.pop_prove sk in
  assert (not (Bls12_381.Signature.Pop.pop_verify pk' proof))

let test_pop_verify_random_proof () =
  let ikm = Bytes.init 32 (fun _i -> char_of_int (Random.int 256)) in
  let pk = Bls12_381.Signature.(derive_pk (generate_sk ikm)) in
  let proof =
    Bytes.init (Bls12_381.G1.size_in_bytes / 2) (fun _i ->
        char_of_int (Random.int 256))
  in
  assert (not (Bls12_381.Signature.Pop.pop_verify pk proof))

let () =
  let open Alcotest in
  run
    "BLS Signature"
    [ BasicProperties.get_tests ();
      AugProperties.get_tests ();
      PopProperties.get_tests ();
      ( "Proof of possession proof/verify properties",
        [ test_case
            "Prove and verify with correct keys"
            `Quick
            (Test_ec_make.repeat 1000 test_pop_prove_verify_with_correct_keys);
          test_case
            "Prove and verify with different pk for verify"
            `Quick
            (Test_ec_make.repeat
               1000
               test_pop_prove_verify_with_different_pk_for_verify);
          test_case
            "Verify random proof"
            `Quick
            (Test_ec_make.repeat 1000 test_pop_verify_random_proof) ] );
      ( "Test vectors from Bls_sigs_ref",
        [ test_case
            "Sign G2 basic from file"
            `Quick
            (TestVectorsSigG2Basic.test_vectors_from_bls_sigs_ref_files
               sig_g2_basic_files);
          test_case
            "Sign G2 Aug from file"
            `Quick
            (TestVectorsSigG2Aug.test_vectors_from_bls_sigs_ref_files
               sig_g2_aug_files);
          test_case
            "Sign G2 pop from file"
            `Quick
            (TestVectorsSigG2Pop.test_vectors_from_bls_sigs_ref_files
               sig_g2_pop_files);
          test_case
            "Pop G2 from file"
            `Quick
            (test_pop_g2_from_blst_sigs_ref_files pop_g2_files) ] ) ]
