c.TextDocument <-
function(..., recursive = FALSE)
{
    args <- list(...)
    x <- args[[1L]]

    if (length(args) == 1L)
        return(x)

    if (!all(unlist(lapply(args, inherits, class(x)))))
        stop("not all arguments are text documents")

    structure(list(content = args,
                   meta = CorpusMeta(),
                   dmeta = data.frame(row.names = seq_along(args))),
              class = c("VCorpus", "Corpus"))
}

PlainTextDocument <-
function(x = character(0),
         author = character(0),
         datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
         description = character(0),
         heading = character(0),
         id = character(0),
         origin = character(0),
         language = character(0),
         ...,
         class = NULL)
{
    structure(list(content = as.character(x),
                   meta = TextDocumentMeta(author, datetimestamp, description,
                                           heading, id, language, origin, ...)),
              class = unique(c(class, "PlainTextDocument", "TextDocument")))
}

as.character.PlainTextDocument <-
function(x, ...)
    content(x)

content.PlainTextDocument <-
function(x)
    x$content

`content<-.PlainTextDocument` <-
function(x, value)
{
    x$content <- as.character(value)
    x
}

print.PlainTextDocument <-
function(x, ...)
{
    writeLines(sprintf("<<PlainTextDocument (metadata: %d)>>", length(x$meta)))
    writeLines(content(x))
    invisible(x)
}

words.PlainTextDocument <-
function(x, ...)
{
    w <- MC_tokenizer(x)
    w[nzchar(w)]
}

XMLTextDocument <-
function(x = list(),
         author = character(0),
         datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
         description = character(0),
         heading = character(0),
         id = character(0),
         origin = character(0),
         language = character(0),
         ...)
{
    structure(list(content = x,
                   meta = TextDocumentMeta(author, datetimestamp, description,
                                           heading, id, language, origin, ...)),
              class = c("XMLTextDocument", "TextDocument"))
}

as.character.XMLTextDocument <-
function(x, ...)
    as.character(unlist(content(x), use.names = FALSE))

content.XMLTextDocument <-
function(x)
    x$content

`content<-.XMLTextDocument` <-
function(x, value)
{
    x$content <- value
    x
}

print.XMLTextDocument <-
function(x, ...)
{
    writeLines(sprintf("<<XMLTextDocument (metadata: %d)>>", length(x$meta)))
    invisible(x)
}
