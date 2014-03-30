#include <R.h>
#include <Rdefines.h>

SEXP copyCorpus(SEXP x, SEXP y) {
    copyVector(x, y);
    return x;
}
