#include "caml/mlvalues.h"
#include "caml/hash.h"
#include "caml/alloc.h"

CAMLprim value
cachet_hash_mix_intnat(value h, value d) {
  return caml_copy_int32(caml_hash_mix_intnat(Int32_val (h), Int_val (d)));
}
