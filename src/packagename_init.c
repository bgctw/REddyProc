#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP REddyProc_RHLightResponseCostC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP REddyProc_whichValueGreaterEqualC(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"REddyProc_RHLightResponseCostC",    (DL_FUNC) &REddyProc_RHLightResponseCostC,    10},
    {"REddyProc_whichValueGreaterEqualC", (DL_FUNC) &REddyProc_whichValueGreaterEqualC,  3},
    {NULL, NULL, 0}
};

void R_init_REddyProc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
