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

# Node ID, actual metadata, and possibly other nodes as children
CorpusMeta <-
function(nodeid = 0,
         value = list(create_date = as.POSIXlt(Sys.time(), tz = "GMT"),
                      creator = Sys.getenv("LOGNAME")),
         children = NULL)
{
    structure(list(nodeid = nodeid, value = value, children = children),
              class = "CorpusMeta")
}

print.CorpusMeta <-
function(x, ...)
    print(x$value)

CorpusDMeta <-
function(x)
    UseMethod("CorpusDMeta", x)
CorpusDMeta.VCorpus <-
function(x)
    x$dmeta
CorpusDMeta.PCorpus <-
function(x)
{
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    result <- filehash::dbFetch(db, "CorpusDMeta")
    index <- x$dmeta[[1, "subset"]]
    if (!any(is.na(index)))
        result <- result[index, , drop = FALSE]
    result
}

`CorpusDMeta<-` <-
function(x, value)
    UseMethod("CorpusDMeta<-", x)
`CorpusDMeta<-.PCorpus` <- function(x, value) {
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    db[["CorpusDMeta"]] <- value
    x$dmeta[[1, "subset"]] <- NA
    x
}
`CorpusDMeta<-.VCorpus` <-
function(x, value)
{
    x$dmeta <- value
    x
}

meta.VCorpus <- meta.PCorpus <-
function(x, tag = NULL, type = c("indexed", "corpus", "local"))
{
    if (!is.null(tag) && missing(type)) {
        type <- if (tag %in% colnames(CorpusDMeta(x)))
            "indexed"
        else if (tag %in% names(x$meta$value))
            "corpus"
        else
            "local"
    }
    type <- match.arg(type)
    if (identical(type, "indexed")) {
        if (is.null(tag))
            CorpusDMeta(x)
        else
            CorpusDMeta(x)[tag]
    } else if (identical(type, "corpus")) {
        if (is.null(tag))
            x$meta
        else
            x$meta$value[[tag]]
    } else if (identical(type, "local"))
        lapply(content(x), meta, tag)
    else
        stop("invalid type")
}
meta.TextDocument <-
function(x, tag = NULL)
{
    if (is.null(tag))
        x$meta
    else
        x$meta[[tag]]
}
meta.TextRepository <- function(x, tag = NULL) {
    if (is.null(tag))
        RepoMetaData(x)
    else
        RepoMetaData(x)[[tag]]
}

`meta<-.VCorpus` <- `meta<-.PCorpus` <-
function(x, tag, type = c("indexed", "corpus", "local"), value)
{
    type <- match.arg(type)
    if (identical(type, "indexed"))
        CorpusDMeta(x)[, tag] <- value
    else if (type == "corpus")
        x$meta$value[[tag]] <- value
    else if (identical(type, "local")) {
        for (i in seq_along(x))
            meta(x[[i]], tag) <- value[i]
    } else
        stop("invalid type")
    x
}
`meta<-.TextDocument` <-
function(x, tag, value)
{
    x$meta[[tag]] <- value
    x
}
`meta<-.TextRepository` <- function(x, tag, value) {
    attr(x, "RepoMetaData")[[tag]] <- value
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
