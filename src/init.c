#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP _rcpp_module_boot_RandomTreeQRN();

static const R_CallMethodDef CallEntries[] = {
  {"_rcpp_module_boot_RandomTreeQRN",   (DL_FUNC) &_rcpp_module_boot_RandomTreeQRN,   0},
  {NULL, NULL, 0}
};

void R_init_treecoding(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
