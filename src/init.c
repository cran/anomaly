#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> 
#include <R_ext/Rdynload.h>


extern SEXP MeanVarAnomaly(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP MeanAnomaly(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"MeanVarAnomaly", (DL_FUNC) &MeanVarAnomaly, 5},
    {"MeanAnomaly", (DL_FUNC) &MeanAnomaly, 5},
    {NULL, NULL, 0}
};

void R_init_anomaly(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
