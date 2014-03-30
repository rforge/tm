# Author: Ingo Feinerer
# Transformations

tm_map <-
function(x, FUN, ...)
    UseMethod("tm_map", x)
tm_map.VCorpus <-
function(x, FUN, ..., useMeta = FALSE, lazy = FALSE)
{
    result <- x
    # Lazy mapping
    if (lazy) {
        lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
        local_fun <- local({
            useMeta <- useMeta
            function(x, ..., dmeta) {
                if (useMeta)
                    FUN(x, ..., dmeta = dmeta)
                else
                    FUN(x, ...)
            }
        })
        if (is.null(lazyTmMap)) {
            meta(result, tag = "lazyTmMap", type = "corpus") <-
                list(index = rep(TRUE, length(result)), maps = list(local_fun))
        }
        else {
            lazyTmMap$maps <- c(lazyTmMap$maps, list(local_fun))
            meta(result, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
        }
    }
    else {
        result$content <- if (useMeta)
                mclapply(content(x), FUN, ..., dmeta = meta(x, type = "indexed"))
            else
                mclapply(content(x), FUN, ...)
    }
    result
}
tm_map.PCorpus <-
function(x, FUN, ..., useMeta = FALSE)
{
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    i <- 1
    for (id in unlist(x$content)) {
        db[[id]] <- if (useMeta)
            FUN(x[[i]], ..., dmeta = meta(x, type = "indexed"))
        else
            FUN(x[[i]], ...)
        i <- i + 1
    }
    # Suggested by Christian Buchta
    filehash::dbReorganize(db)

    x
}

# Materialize lazy mappings
# Improvements by Christian Buchta
materialize <-
function(x, range = seq_along(x))
{
    lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(x) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- x$content[idx]
           for (m in lazyTmMap$maps)
               res <- lapply(res, m, dmeta = meta(x, type = "indexed"))
           x$content[idx] <- res
           lazyTmMap$index[idx] <- FALSE
       }
    }
    # Clean up if everything is materialized
    if (!any(lazyTmMap$index))
        lazyTmMap <- NULL
    meta(x, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
    x
}

tm_reduce <- function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

getTransformations <- function()
    c("as.PlainTextDocument", "removeNumbers", "removePunctuation",
      "removeWords", "stemDocument", "stripWhitespace")

# Wrapper for transformation generation
genMap <-
function(FUN)
    function(x, ...) {
        content(x) <- FUN(content(x), ...)
        x
    }

removeNumbers <-
function(x)
    UseMethod("removeNumbers", x)
removeNumbers.character <-
function(x)
    gsub("[[:digit:]]+", "", x)
removeNumbers.PlainTextDocument <- genMap(removeNumbers.character)

removePunctuation <-
function(x, preserve_intra_word_dashes = FALSE)
    UseMethod("removePunctuation", x)
removePunctuation.character <-
function(x, preserve_intra_word_dashes = FALSE)
{
    if (!preserve_intra_word_dashes)
        gsub("[[:punct:]]+", "", x)
    else {
        # Assume there are no ASCII 1 characters.
        x <- gsub("(\\w)-(\\w)", "\\1\1\\2", x)
        x <- gsub("[[:punct:]]+", "", x)
        gsub("\1", "-", x, fixed = TRUE)
    }
}
removePunctuation.PlainTextDocument <- genMap(removePunctuation.character)

removeWords <-
function(x, words)
    UseMethod("removeWords", x)
# Improvements by Kurt Hornik
removeWords.character <-
function(x, words)
    gsub(sprintf("(*UCP)\\b(%s)\\b", paste(words, collapse = "|")), "", x,
         perl = TRUE)
removeWords.PlainTextDocument <- genMap(removeWords.character)

stemDocument <-
function(x, language = "english")
    UseMethod("stemDocument", x)
stemDocument.character <-
function(x, language = "english")
    SnowballC::wordStem(x, language)
stemDocument.PlainTextDocument <-
function(x, language = meta(x, "language"))
{
    s <- unlist(lapply(content(x),
      function(x) paste(stemDocument.character(unlist(strsplit(x, "[[:blank:]]")),
                                               language),
                        collapse = " ")))
    content(x) <- if (is.character(s)) s else ""
    x
}

stripWhitespace <-
function(x)
    UseMethod("stripWhitespace", x)
stripWhitespace.character <-
function(x)
    gsub("[[:space:]]+", " ", x)
stripWhitespace.PlainTextDocument <- genMap(stripWhitespace.character)
