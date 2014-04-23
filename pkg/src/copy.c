#include <R.h>
#include <Rdefines.h>

void copyCorpus(SEXP x, SEXP y) {
    copyVector(x, y);
}
