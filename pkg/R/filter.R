# Author: Ingo Feinerer
# Filters

tm_filter <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE) UseMethod("tm_filter", x)
tm_filter.Corpus <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE)
    x[tm_index(x, ..., FUN = FUN, doclevel = doclevel, useMeta = useMeta)]

tm_index <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE) UseMethod("tm_index", x)
tm_index.Corpus <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE) {
    if (!is.null(attr(FUN, "doclevel")))
        doclevel <- attr(FUN, "doclevel")
    if (doclevel) {
        if (useMeta)
            return(unlist(parallel::mclapply(x, FUN, ..., DMetaData = DMetaData(x))))
        else
            return(unlist(parallel::mclapply(x, FUN, ...)))
    }
    else
        return(FUN(x, ...))
}
