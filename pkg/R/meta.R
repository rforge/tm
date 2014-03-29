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
    cat("Metadata:\n")
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
    else if (identical(type, "local")) {
        lapply(seq_along(x), function(i) meta(x[[i]], tag))
        # TODO: If content(x) returns the actual documents we could also use
        # lapply(content(x), meta, tag)
    } else
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

# Simple Dublin Core to tm meta data mappings
# http://en.wikipedia.org/wiki/Dublin_core#Simple_Dublin_Core
Dublin_Core_tm <-
function(DCElem = c("title", "creator", "description", "date", "identifier", "language", "subject",
         "publisher", "contributor", "type", "format", "source", "relation", "coverage", "rights"))
{
    DCElem <- tolower(DCElem)
    DCElem <- match.arg(DCElem)
    if (identical(DCElem, "title")) return(list(tag = "heading", type = "local"))
    if (identical(DCElem, "creator")) return(list(tag = "author", type = "local"))
    if (identical(DCElem, "description")) return(list(tag = "description", type = "local"))
    if (identical(DCElem, "date")) return(list(tag = "datetimestamp", type = "local"))
    if (identical(DCElem, "identifier")) return(list(tag = "id", type = "local"))
    if (identical(DCElem, "language")) return(list(tag = "language", type = "local"))
    # Source -> Origin ?

    if (identical(DCElem, "subject") || identical(DCElem, "publisher") || identical(DCElem, "contributor") ||
        identical(DCElem, "type") || identical(DCElem, "format") || identical(DCElem, "source") ||
        identical(DCElem, "relation") || identical(DCElem, "coverage") || identical(DCElem, "rights"))
        return(list(tag = DCElem, type = "extended"))

    stop("invalid simple Dublin Core meta data element")
}

DublinCore <- function(x, tag = NULL) {
    if (is.null(tag)) {
        elements <- c("Title", "Creator", "Subject", "Description", "Publisher",
                      "Contributor", "Date", "Type", "Format", "Identifier",
                      "Source", "Language", "Relation", "Coverage", "Rights")
        cat("Simple Dublin Core meta data pairs are:\n")
        for (e in elements) {
            DCtm <- Dublin_Core_tm(e)
            DCvalue <- meta(x, DCtm$tag)
            cat(sprintf("  %-11s: %s\n", e, paste(as.character(DCvalue), collapse = " ")))
        }
    }
    else {
        DCtm <- Dublin_Core_tm(tag)
        meta(x, DCtm$tag)
    }
}

`DublinCore<-` <- function(x, tag, value) {
    DCtm <- Dublin_Core_tm(tag)
    meta(x, DCtm$tag) <- value
    x
}
