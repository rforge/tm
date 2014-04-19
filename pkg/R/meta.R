# Author: Ingo Feinerer

TextDocumentMeta <-
function(author, datetimestamp, description, heading, id, language, origin, ...)
{
    structure(list(author = as.person(author),
                   datetimestamp = as.POSIXlt(datetimestamp, tz = "GMT"),
                   description = as.character(description),
                   heading = as.character(heading),
                   id = as.character(id),
                   language = as.character(language),
                   origin = as.character(origin),
                   ...),
              class = "TextDocumentMeta")
}

print.TextDocumentMeta <-
function(x, ...)
{
    writeLines("Metadata:")
    cat(sprintf("  %s: %s",
                format(names(x), justify = "left"),
                sapply(x, as.character)),
        sep = "\n")
    invisible(x)
}

CorpusMeta <-
function(...)
    structure(list(...), class = "CorpusMeta")

meta.VCorpus <- meta.PCorpus <-
function(x, tag = NULL, type = c("indexed", "corpus", "local"), ...)
{
    if (!is.null(tag) && missing(type)) {
        type <- if (tag %in% colnames(x$dmeta)) "indexed"
        else if (tag %in% names(x$meta)) "corpus"
        else "local"
    }
    type <- match.arg(type)
    if (identical(type, "indexed"))
        if (is.null(tag)) x$dmeta else x$dmeta[tag]
    else if (identical(type, "corpus"))
        if (is.null(tag)) x$meta else x$meta[[tag]]
    else if (identical(type, "local"))
        lapply(x, meta, tag)
    else
        stop("invalid type")
}
meta.TextDocument <-
function(x, tag = NULL, ...)
    if (is.null(tag)) x$meta else x$meta[[tag]]

`meta<-.VCorpus` <- `meta<-.PCorpus` <-
function(x, tag, type = c("indexed", "corpus", "local"), ..., value)
{
    type <- match.arg(type)
    if (identical(type, "indexed"))
        x$dmeta[, tag] <- value
    else if (type == "corpus")
        x$meta[[tag]] <- value
    else if (identical(type, "local")) {
        for (i in seq_along(x))
            meta(x[[i]], tag) <- value[i]
    } else
        stop("invalid type")
    x
}
`meta<-.TextDocument` <-
function(x, tag, ..., value)
{
    x$meta[[tag]] <- value
    x
}

# Simple Dublin Core to tm metadata mapping
# http://en.wikipedia.org/wiki/Dublin_core#Simple_Dublin_Core
Dublin_Core_tm_map <-
list("contributor" = "contributor",
     "coverage" = "coverage",
     "creator" = "author",
     "date" = "datetimestamp",
     "description" = "description",
     "format" = "format",
     "identifier" = "id",
     "language" = "language",
     "publisher" = "publisher",
     "relation" = "relation",
     "rights" = "rights",
     "source" = "source", # or better "origin"?
     "subject" = "subject",
     "title" = "heading",
     "type" = "type"
     )

DublinCore <-
function(x, tag = NULL)
{
    tmm <- unlist(Dublin_Core_tm_map, use.names = FALSE)
    dcm <- names(Dublin_Core_tm_map)

    if (is.null(tag))
        structure(lapply(tmm, function(t) meta(x, t)), names = dcm,
                  class = "TextDocumentMeta")
    else
        meta(x, tmm[charmatch(tolower(tag), dcm)])
}

`DublinCore<-` <-
function(x, tag, value)
{
    tmm <- unlist(Dublin_Core_tm_map, use.names = FALSE)
    dcm <- names(Dublin_Core_tm_map)

    meta(x, tmm[charmatch(tolower(tag), dcm)]) <- value
    x
}
