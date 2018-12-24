#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include <omp.h>

extern SEXP c_llc_c(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  const double ll = asReal(l);
  const double aa = asReal(a);
  double *px = REAL(x);
  double llc = 0.0;
  for (int i = 0; i < n; ++i) {
    llc += fmax(0.0, fmin(px[i] - aa, ll));
  }
  SEXP ret = PROTECT(allocVector(REALSXP, 1));
  REAL(ret)[0] = llc;
  UNPROTECT(1);
  return(ret);
}

extern SEXP c_llc_cs(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  const double ll = asReal(l);
  const double aa = asReal(a);
  double *px = REAL(x);
  double llc = 0.0;
#pragma omp simd reduction(+:llc)  
  for (int i = 0; i < n; ++i) {
    llc += fmax(0.0, fmin(px[i] - aa, ll));
  }
  SEXP ret = PROTECT(allocVector(REALSXP, 1));
  REAL(ret)[0] = llc;
  UNPROTECT(1);
  return(ret);
}

extern SEXP c_llc_cl(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  const double ll = asReal(l);
  const double aa = asReal(a);
  double *px = REAL(x);
  double llc = 0.0;
#pragma omp parallel for schedule(static), reduction(+:llc)  
  for (int i = 0; i < n; ++i) {
    llc += fmax(0.0, fmin(px[i] - aa, ll));
  }
  SEXP ret = PROTECT(allocVector(REALSXP, 1));
  REAL(ret)[0] = llc;
  UNPROTECT(1);
  return(ret);
}

extern SEXP c_llc_cls(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  const double ll = asReal(l);
  const double aa = asReal(a);
  double *px = REAL(x);
  double llc = 0.0;
#pragma omp parallel for simd schedule(static), reduction(+:llc)  
  for (int i = 0; i < n; ++i) {
    llc += fmax(0.0, fmin(px[i] - aa, ll));
  }
  SEXP ret = PROTECT(allocVector(REALSXP, 1));
  REAL(ret)[0] = llc;
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(llc_fd)(double *x, int n, double *l, double *a, double *ret);

extern SEXP c_llc_fd(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(llc_fd)(REAL(x), n, REAL(l), REAL(a), REAL(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(llc_fe)(double *x, int n, double *l, double *a, double *ret);

extern SEXP c_llc_fe(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(llc_fe)(REAL(x), n, REAL(l), REAL(a), REAL(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(llc_f)(double *x, int n, double *l, double *a, double *ret);

extern SEXP c_llc_f(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(llc_f)(REAL(x), n, REAL(l), REAL(a), REAL(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(llc_fs)(double *x, int n, double *l, double *a, double *ret);

extern SEXP c_llc_fs(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(llc_fs)(REAL(x), n, REAL(l), REAL(a), REAL(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(llc_fl)(double *x, int n, double *l, double *a, double *ret);

extern SEXP c_llc_fl(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(llc_fl)(REAL(x), n, REAL(l), REAL(a), REAL(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(llc_fls)(double *x, int n, double *l, double *a, double *ret);

extern SEXP c_llc_fls(SEXP x, SEXP l, SEXP a){
  const int n = LENGTH(x);
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(llc_fls)(REAL(x), n, REAL(l), REAL(a), REAL(ret));
  UNPROTECT(1);
  return(ret);
}

static const R_CallMethodDef CallEntries[] = {
  {"c_llc_fd",   (DL_FUNC) &c_llc_fd,   3},
  {"c_llc_fe",   (DL_FUNC) &c_llc_fe,   3},
  {"c_llc_f",    (DL_FUNC) &c_llc_f,    3},
  {"c_llc_fs",   (DL_FUNC) &c_llc_fs,   3},
  {"c_llc_fl",   (DL_FUNC) &c_llc_fl,   3},
  {"c_llc_fls",  (DL_FUNC) &c_llc_fls,  3},
  {"c_llc_c",    (DL_FUNC) &c_llc_c,    3},
  {"c_llc_cs",   (DL_FUNC) &c_llc_cs,   3},
  {"c_llc_cl",   (DL_FUNC) &c_llc_cl,   3},
  {"c_llc_cls",  (DL_FUNC) &c_llc_cls,  3},
  {NULL,         NULL,                  0}
};

void R_init_RFortLangComp(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}