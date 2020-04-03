#include <stdio.h>
#include "liblys.h"

void build_text(const struct lys_context *ctx, char* dest, size_t dest_len, const char* format, float render_milliseconds, char* **sum_names) {
  union { uint32_t val; char* sum_name; } out0;
  union { uint32_t val; char* sum_name; } out1;
  union { uint32_t val; char* sum_name; } out2;
  union { float val; char* sum_name; } out3;
  union { float val; char* sum_name; } out4;
  union { uint32_t val; char* sum_name; } out5;
  FUT_CHECK(ctx->fut, futhark_entry_text_content(ctx->fut, &out0.val, &out1.val, &out2.val, &out3.val, &out4.val, &out5.val, render_milliseconds, ctx->state));
  if (sum_names[0] != NULL) {
    out0.sum_name = sum_names[0][(int32_t) out0.val];
  }
  if (sum_names[1] != NULL) {
    out1.sum_name = sum_names[1][(int32_t) out1.val];
  }
  if (sum_names[2] != NULL) {
    out2.sum_name = sum_names[2][(int32_t) out2.val];
  }
  if (sum_names[3] != NULL) {
    out3.sum_name = sum_names[3][(int32_t) out3.val];
  }
  if (sum_names[4] != NULL) {
    out4.sum_name = sum_names[4][(int32_t) out4.val];
  }
  if (sum_names[5] != NULL) {
    out5.sum_name = sum_names[5][(int32_t) out5.val];
  }
  snprintf(dest, dest_len, format, out0.val, out1.val, out2.val, out3.val, out4.val, out5.val);
}

size_t n_printf_arguments() {
  return 6;
}
