#include "poseidon128.h"
#include "ark_128.h"
#include "mds_128.h"
#include <stdlib.h>
#include <string.h>

blst_fr ARK[NB_CONSTANTS];
blst_fr MDS[WIDTH][WIDTH];

blst_fr *buffer;
blst_fr *res_mds[WIDTH];

uint64_t ZERO[4] = {0, 0, 0, 0};

void poseidon128_constants_init(void) {
  buffer = (blst_fr *)calloc(1, sizeof(blst_scalar));
  for (int i = 0; i < WIDTH; i++) {
    res_mds[i] = (blst_fr *)calloc(1, sizeof(blst_scalar));
  }

  blst_scalar *scalar = (blst_scalar *)calloc(1, sizeof(blst_scalar));
  for (int i = 0; i < NB_CONSTANTS; i++) {
    blst_scalar_from_lendian(scalar, ARK_BYTES[i]);
    blst_fr_from_scalar(ARK + i, scalar);
  }
  for (int i = 0; i < WIDTH; i++) {
    for (int j = 0; j < WIDTH; j++) {
      blst_scalar_from_lendian(scalar, MDS_BYTES[i][j]);
      blst_fr_from_scalar(&MDS[i][j], scalar);
    }
  }
  free(scalar);
}

void poseidon128_finalize(void) {
  free(buffer);
  for (int i = 0; i < WIDTH; i++) {
    free(res_mds[i]);
  }
}

void poseidon128_init(poseidon128_ctxt_t *ctxt, blst_fr *a, blst_fr *b,
                      blst_fr *c) {
  memcpy(&ctxt->s[0], a, sizeof(blst_fr));
  memcpy(&ctxt->s[1], b, sizeof(blst_fr));
  memcpy(&ctxt->s[2], c, sizeof(blst_fr));
}

void apply_sbox(poseidon128_ctxt_t *ctxt, blst_fr *buffer, int full) {
  int begin_idx = full ? 0 : PARTIAL_ROUND_IDX_SBOX;
  int end_idx = full ? WIDTH : PARTIAL_ROUND_IDX_SBOX + 1;
  for (int i = begin_idx; i < end_idx; i++) {
    // x * (x^2)^2
    blst_fr_sqr(buffer, &ctxt->s[i]);
    blst_fr_sqr(buffer, buffer);
    blst_fr_mul(&ctxt->s[i], buffer, &ctxt->s[i]);
  }
}

void apply_matrix_multiplication(poseidon128_ctxt_t *ctxt, blst_fr *buffer,
                                 blst_fr *res[WIDTH]) {
  for (int i = 0; i < WIDTH; i++) {
    blst_fr_from_uint64(res[i], ZERO);
    for (int j = 0; j < WIDTH; j++) {
      blst_fr_mul(buffer, &MDS[i][j], &ctxt->s[j]);
      blst_fr_add(res[i], res[i], buffer);
    }
  }
  for (int i = 0; i < WIDTH; i++) {
    memcpy(&ctxt->s[i], res[i], sizeof(blst_fr));
  }
}

void apply_cst(poseidon128_ctxt_t *ctxt) {
  for (int i = 0; i < WIDTH; i++) {
    blst_fr_add(&ctxt->s[i], &ctxt->s[i], ARK + ctxt->i_round_key);
    ctxt->i_round_key++;
  }
}

void poseidon128_apply_perm(poseidon128_ctxt_t *ctxt) {
  // Could we allocate it outside of apply_perm?
  ctxt->i_round_key = 0;
  for (int i = 0; i < NB_FULL_ROUNDS / 2; i++) {
    apply_cst(ctxt);
    apply_sbox(ctxt, buffer, 1);
    apply_matrix_multiplication(ctxt, buffer, res_mds);
  }
  for (int i = 0; i < NB_PARTIAL_ROUNDS; i++) {
    apply_cst(ctxt);
    apply_sbox(ctxt, buffer, 0);
    apply_matrix_multiplication(ctxt, buffer, res_mds);
  }
  for (int i = 0; i < NB_FULL_ROUNDS / 2; i++) {
    apply_cst(ctxt);
    apply_sbox(ctxt, buffer, 1);
    apply_matrix_multiplication(ctxt, buffer, res_mds);
  }
}

void poseidon128_get_state(blst_fr *a, blst_fr *b, blst_fr *c,
                           poseidon128_ctxt_t *ctxt) {
  memcpy(a, &ctxt->s[0], sizeof(blst_fr));
  memcpy(b, &ctxt->s[1], sizeof(blst_fr));
  memcpy(c, &ctxt->s[2], sizeof(blst_fr));
}
