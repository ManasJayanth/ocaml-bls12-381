#ifndef POSEIDON128_H
#define POSEIDON128_H

#include "blst.h"

#define WIDTH 3
#define NB_FULL_ROUNDS 8
#define NB_PARTIAL_ROUNDS 58
#define PARTIAL_ROUND_IDX_SBOX 0
#define NB_CONSTANTS ((NB_FULL_ROUNDS + NB_PARTIAL_ROUNDS) * 3)

typedef struct poseidon128_ctxt_s {
  blst_fr s[WIDTH];
  int i_round_key;
} poseidon128_ctxt_t;

void poseidon128_constants_init(void);
void poseidon128_finalize(void);
void poseidon128_init(poseidon128_ctxt_t *ctxt, blst_fr *a, blst_fr *b,
                      blst_fr *c);
void poseidon128_apply_perm(poseidon128_ctxt_t *ctxt);
void poseidon128_get_state(blst_fr *a, blst_fr *b, blst_fr *c,
                           poseidon128_ctxt_t *ctxt);
#endif
