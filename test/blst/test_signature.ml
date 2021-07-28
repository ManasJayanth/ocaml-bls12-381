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
