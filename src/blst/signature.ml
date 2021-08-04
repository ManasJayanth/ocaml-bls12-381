module Stubs = Blst_bindings.StubsSignature (Blst_stubs)
module StubsFr = Blst_bindings.StubsFr (Blst_stubs)
module StubsG1 = Blst_bindings.StubsG1 (Blst_stubs)
module StubsG2 = Blst_bindings.StubsG2 (Blst_stubs)

type signature = Bytes.t

let check_unicity_lst lst =
  let rec aux tbl lst =
    match lst with
    | [] -> true
    | hd :: tl ->
        if Hashtbl.mem tbl hd then false
        else (
          Hashtbl.add tbl hd 0 ;
          aux tbl tl )
  in
  let tbl = Hashtbl.create (List.length lst) in
  aux tbl lst

let allocate_blst_pairing_t () =
  let allocated_memory = Stubs.malloc (Stubs.sizeof_pairing ()) in
  Ctypes.(
    coerce (ptr void) (ptr Blst_bindings.Types.blst_pairing_t) allocated_memory)

type sk = Blst_bindings.Types.blst_scalar_t Ctypes.ptr

type pk = G1.t

let sk_of_bytes_exn bytes =
  let buffer = Blst_bindings.Types.allocate_scalar () in
  if Bytes.length bytes > 32 then
    raise
      (Invalid_argument
         "Input should be maximum 32 bytes, encoded the secret key in little \
          endian")
  else
    let sk = Fr.of_bytes_exn bytes in
    StubsFr.scalar_of_fr buffer sk ;
    buffer

let sk_to_bytes sk =
  let bytes = Bytes.make 32 '\000' in
  StubsFr.scalar_to_bytes_le (Ctypes.ocaml_bytes_start bytes) sk ;
  bytes

let generate_sk ?(key_info = Bytes.empty) ikm =
  let buffer_scalar = Blst_bindings.Types.allocate_scalar () in
  let key_info_length = Bytes.length key_info in
  let ikm_length = Bytes.length ikm in

  (*
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04
    Section 2.3 - KeyGen

    For security, IKM MUST be infeasible to guess, e.g., generated by a
    trusted source of randomness.  IKM MUST be at least 32 bytes long,
    but it MAY be longer.

    Also, blst_keygen returns a vector of zero (commit
    095a8c53787d6c91b725152ebfbbf33acf05a931) if ikm is less than 32 bytes
  *)
  assert (ikm_length >= 32) ;
  Stubs.keygen
    buffer_scalar
    (Ctypes.ocaml_bytes_start ikm)
    (Unsigned.Size_t.of_int ikm_length)
    (Ctypes.ocaml_bytes_start key_info)
    (Unsigned.Size_t.of_int key_info_length) ;
  buffer_scalar

let derive_pk sk =
  let buffer_g1 = Blst_bindings.Types.allocate_g1 () in
  Stubs.sk_to_pk buffer_g1 sk ;
  buffer_g1

let pk_of_bytes pk_bytes = G1.of_bytes_opt pk_bytes

let core_sign sk message dst =
  let hash = G2.hash_to_curve message dst in
  let buffer = Blst_bindings.Types.allocate_g2 () in
  Stubs.sign buffer hash sk ;
  G2.to_compressed_bytes buffer

let core_verify pk msg signature dst =
  let signature_opt = G2.of_compressed_bytes_opt signature in
  match signature_opt with
  | None -> false
  | Some signature ->
      let dst_length = Bytes.length dst in
      let ctxt = allocate_blst_pairing_t () in
      Stubs.pairing_init
        ctxt
        true
        (Ctypes.ocaml_bytes_start dst)
        (Unsigned.Size_t.of_int dst_length) ;
      let msg_length = Bytes.length msg in
      let signature_affine = Blst_bindings.Types.allocate_g2_affine () in
      StubsG2.to_affine signature_affine signature ;
      let pk_affine = Blst_bindings.Types.allocate_g1_affine () in
      StubsG1.to_affine pk_affine pk ;
      let res =
        Stubs.pairing_chk_n_mul_n_aggr_pk_in_g1
          ctxt
          pk_affine
          (* Does not check pk is a point on the curve and in the subgroup, it
             is verified by the OCaml type
          *)
          false
          signature_affine
          (* Does not check signature is a point on the curve and in the subgroup, it
             is verified by the OCaml type
          *)
          false
          (Ctypes.ocaml_bytes_start Bytes.empty)
          Unsigned.Size_t.zero
          (Ctypes.ocaml_bytes_start msg)
          (Unsigned.Size_t.of_int msg_length)
          (Ctypes.ocaml_bytes_start Bytes.empty)
          Unsigned.Size_t.zero
      in
      if res = 0 then (
        Stubs.pairing_commit ctxt ;
        Stubs.pairing_finalverify
          ctxt
          Ctypes.(from_voidp Blst_bindings.Types.blst_fq12_t null) )
      else false

let aggregate_signature_opt signatures =
  let rec aux signatures acc =
    match signatures with
    | [] -> Some acc
    | signature :: signatures -> (
        let signature = G2.of_compressed_bytes_opt signature in
        match signature with
        | None -> None
        | Some signature ->
            let acc = G2.(add signature acc) in
            aux signatures acc )
  in
  let res = aux signatures G2.zero in
  match res with None -> None | Some res -> Some (G2.to_compressed_bytes res)

let core_aggregate_verify pks_with_msgs aggregated_signature dst =
  let rec aux aggregated_signature pks_with_msgs ctxt =
    match pks_with_msgs with
    | (pk, msg) :: rest ->
        (* sign the message *)
        let aggregated_signature =
          match aggregated_signature with
          | None ->
              Ctypes.(from_voidp Blst_bindings.Types.blst_g2_affine_t null)
          | Some aggregated_signature ->
              let signature_affine =
                Blst_bindings.Types.allocate_g2_affine ()
              in
              StubsG2.to_affine signature_affine aggregated_signature ;
              signature_affine
        in
        let pk_affine = Blst_bindings.Types.allocate_g1_affine () in
        StubsG1.to_affine pk_affine pk ;
        let msg_length = Bytes.length msg in
        let res =
          Stubs.pairing_chk_n_mul_n_aggr_pk_in_g1
            ctxt
            pk_affine
            (* Does not check pk is a point on the curve and in the subgroup, it
               is verified by the OCaml type
            *)
            false
            (* signature: must be null except the first one *)
            aggregated_signature
            (* Does not check signature is a point on the curve and in the subgroup, it
               is verified by the OCaml type
            *)
            false
            (* scalar *)
            (Ctypes.ocaml_bytes_start Bytes.empty)
            Unsigned.Size_t.zero
            (* msg *)
            (Ctypes.ocaml_bytes_start msg)
            (Unsigned.Size_t.of_int msg_length)
            (* aug *)
            (Ctypes.ocaml_bytes_start Bytes.empty)
            Unsigned.Size_t.zero
        in
        assert (res = 0) ;
        aux None rest ctxt
    | [] -> ()
  in
  let aggregated_signature_opt =
    G2.of_compressed_bytes_opt aggregated_signature
  in
  match aggregated_signature_opt with
  | None -> false
  | Some aggregated_signature ->
      let ctxt = allocate_blst_pairing_t () in
      Stubs.pairing_init
        ctxt
        true
        (Ctypes.ocaml_bytes_start dst)
        (Unsigned.Size_t.of_int (Bytes.length dst)) ;
      aux (Some aggregated_signature) pks_with_msgs ctxt ;
      Stubs.pairing_commit ctxt ;
      Stubs.pairing_finalverify
        ctxt
        Ctypes.(from_voidp Blst_bindings.Types.blst_fq12_t null)

module Basic = struct
  let dst = Bytes.of_string "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"

  let dst_length = Bytes.length dst

  let sign sk message = core_sign sk message dst

  let verify pk msg signature = core_verify pk msg signature dst

  let aggregate_verify pks_with_msgs aggregated_signature =
    let msgs = List.map snd pks_with_msgs in
    if check_unicity_lst msgs then
      core_aggregate_verify pks_with_msgs aggregated_signature dst
    else raise (Invalid_argument "Messages must be distinct")
end

module Aug = struct
  let dst = Bytes.of_string "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_AUG_"

  let dst_length = Bytes.length dst

  let sign sk msg =
    let pk = derive_pk sk in
    (* Important note: we concatenate with the compressed representation of the
       point!
    *)
    let pk_bytes = G1.to_compressed_bytes pk in
    let msg = Bytes.concat Bytes.empty [pk_bytes; msg] in
    core_sign sk msg dst

  let verify pk msg signature =
    (* Important note: we concatenate with the compressed representation of the
       point!
    *)
    let pk_bytes = G1.to_compressed_bytes pk in
    let msg = Bytes.concat Bytes.empty [pk_bytes; msg] in
    core_verify pk msg signature dst

  let aggregate_verify pks_with_msgs aggregated_signature =
    let pks_with_msgs =
      List.map
        (fun (pk, msg) ->
          (pk, Bytes.concat Bytes.empty [G1.to_compressed_bytes pk; msg]))
        pks_with_msgs
    in
    core_aggregate_verify pks_with_msgs aggregated_signature dst
end

module Pop = struct
  type proof = Bytes.t

  let sign sk message =
    let dst = Bytes.of_string "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_" in
    core_sign sk message dst

  let verify pk msg signature =
    let dst = Bytes.of_string "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_" in
    core_verify pk msg signature dst

  let pop_prove sk =
    let dst = Bytes.of_string "BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_" in
    let pk = derive_pk sk in
    let pk_bytes = G1.to_compressed_bytes pk in
    core_sign sk pk_bytes dst

  let pop_verify pk signature =
    let dst = Bytes.of_string "BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_" in
    let msg = G1.to_compressed_bytes pk in
    core_verify pk msg signature dst

  let aggregate_verify pks_with_pops msg aggregated_signature =
    let pks = List.map fst pks_with_pops in
    let aggregated_pk = List.fold_left G1.add G1.zero pks in
    let signature_check = verify aggregated_pk msg aggregated_signature in
    let pop_checks =
      List.for_all
        (fun (pk, signature) -> pop_verify pk signature)
        pks_with_pops
    in
    pop_checks && signature_check
end
