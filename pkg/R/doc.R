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

as.PlainTextDocument <-
function(x)
    UseMethod("as.PlainTextDocument", x)

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
    cat(noquote(content(x)), sep = "\n")
    invisible(x)
}

words.PlainTextDocument <-
function(x, ...)
    unique(MC_tokenizer(content(x)))

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

`content<-.XMLTextDocument` <-
function(x, value)
{
    x$content <- value
    x
}
