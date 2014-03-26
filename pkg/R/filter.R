# Author: Ingo Feinerer
# Filters

tm_filter <-
function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE)
    UseMethod("tm_filter", x)
tm_filter.Corpus <-
function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE)
    x[tm_index(x, ..., FUN = FUN, doclevel = doclevel, useMeta = useMeta)]

tm_index <-
function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE)
    UseMethod("tm_index", x)
tm_index.Corpus <-
function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE)
{
    if (!is.null(attr(FUN, "doclevel")))
        doclevel <- attr(FUN, "doclevel")
    if (doclevel) {
        if (useMeta)
            unlist(mclapply(content(x), FUN, ...,
                                      dmeta = meta(x, type = "indexed")))
        else
            unlist(mclapply(content(x), FUN, ...))
    }
    else
        FUN(x, ...)
}
