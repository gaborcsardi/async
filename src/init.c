
#include "async.h"

static const R_CallMethodDef callMethods[]  = {
  { "async_poll", (DL_FUNC) &async_poll, 2 },
  { NULL, NULL, 0 }
};

void R_init_async(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
