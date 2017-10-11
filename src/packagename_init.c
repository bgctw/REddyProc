#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _REddyProc_RHLightResponseCostC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _REddyProc_whichValueGreaterEqualC(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_REddyProc_RHLightResponseCostC",    (DL_FUNC) &_REddyProc_RHLightResponseCostC,    10},
    {"_REddyProc_whichValueGreaterEqualC", (DL_FUNC) &_REddyProc_whichValueGreaterEqualC,  3},
    {NULL, NULL, 0}
};

void R_init_REddyProc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
