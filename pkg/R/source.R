## Author: Ingo Feinerer
## Sources

getSources <-
function()
   c("DataframeSource", "DirSource", "ReutersSource", "URISource",
     "VectorSource")

SimpleSource <-
function(defaultreader = readPlain,
         encoding = "unknown",
         length = NA_integer_,
         names = NA_character_,
         position = 0,
         vectorized = TRUE,
         ...,
         class)
{
    if (!is.function(defaultreader))
        stop("invalid default reader")
    else if (!is.character(encoding))
        warning("invalid encoding")
    else if (!is.integer(length))
        warning("invalid length entry denoting the number of elements")
    else if (!is.character(names) && !is.null(names))
        warning("invalid element names")
    else if (!is.numeric(position))
        warning("invalid position")
    else if (!is.logical(vectorized))
        warning("invalid indicator for parallel element access")
    else if (isTRUE(vectorized) && (is.na(length) || length <= 0L))
        warning("vectorized sources must have a positive length entry")
    else if (!is.null(names) && !is.na(names) && (length != length(names)))
        warning("incorrect number of element names")

    structure(list(defaultreader = defaultreader, encoding = encoding,
                   length = length, names = names, position = position,
                   vectorized = vectorized, ...),
              class = unique(c(class, "SimpleSource", "Source")))
}

# A vector where each component is interpreted as document
VectorSource <-
function(x, encoding = "unknown")
    SimpleSource(encoding = encoding, length = length(x), names = names(x),
                 content = if (is.factor(x)) as.character(x) else x,
                 class = "VectorSource")

# A data frame where each row is interpreted as document
DataframeSource <-
function(x, encoding = "unknown")
    SimpleSource(encoding = encoding, length = nrow(x), names = row.names(x),
                 content = if (is.factor(x)) as.character(x) else x,
                 class = "DataframeSource")

# A directory with files
DirSource <-
function(directory = ".", encoding = "unknown", pattern = NULL,
         recursive = FALSE, ignore.case = FALSE, mode = "text")
{
    if (!identical(mode, "text") &&
        !identical(mode, "binary") &&
        !identical(mode, ""))
        stop(sprintf("invalid mode '%s'", mode))

    d <- dir(directory, full.names = TRUE, pattern = pattern,
             recursive = recursive, ignore.case = ignore.case)

    if (!length(d))
        stop("empty directory")

    isfile <- !file.info(d)[["isdir"]]
    if (any(is.na(isfile)))
        stop("non-existent or non-readable file(s): ",
             paste(d[is.na(isfile)], collapse = " "))

    SimpleSource(encoding = encoding, length = sum(isfile),
                 names = basename(d[isfile]), mode = mode,
                 filelist = d[isfile], class = "DirSource")
}

# Documents identified by a Uniform Resource Identifier
URISource <-
function(x, encoding = "unknown", mode = "text")
{
    if (!identical(mode, "text") &&
        !identical(mode, "binary") &&
        !identical(mode, ""))
        stop(sprintf("invalid mode '%s'", mode))

    SimpleSource(encoding = encoding, length = length(x), uri = x, mode = mode,
                 class = "URISource")
}

ReutersSource <- function(x, encoding = "unknown")
    XMLSource(x, function(tree) XML::xmlChildren(XML::xmlRoot(tree)),
              readReut21578XML, encoding)

# XML
XMLSource <- function(x, parser, reader, encoding = "unknown") {
    tree <- XML::xmlParse(x, encoding = encoding)
    content <- parser(tree)
    XML::free(tree)

    SimpleSource(defaultreader = reader, encoding = encoding,
                 length = length(content), vectorized = FALSE,
                 content = content, uri = x, class = "XMLSource")
}

stepNext <- function(x) UseMethod("stepNext", x)
stepNext.SimpleSource <- function(x) {
    x$position <- x$position + 1
    x
}

# tau:::read_all_bytes
read_all_bytes <-
function(con, chunksize = 2 ^ 16)
{
    if(is.character(con)) {
        return(readBin(con, raw(), file.info(con)$size))
    }

    if(!isOpen(con)) {
        open(con, "rb")
        on.exit(close(con))
    }

    bytes <- list()
    repeat {
        chunk <- readBin(con, raw(), chunksize)
        bytes <- c(bytes, list(chunk))
        if(length(chunk) < chunksize) break
    }

    unlist(bytes)
}

readContent <-
function(x, encoding, mode)
{
    if (identical(mode, "text"))
        readLines(x, encoding = encoding)
    else if (identical(mode, "binary"))
        read_all_bytes(x)
    else if (identical(mode, ""))
        NULL
    else
        stop("invalid mode")
}

getElem <- function(x) UseMethod("getElem", x)
getElem.DataframeSource <-
function(x)
    list(content = x$content[x$position, ],
         uri = NULL)
getElem.DirSource <-
function(x)
{
    filename <- x$filelist[x$position]
    list(content = readContent(filename, x$encoding, x$mode),
         uri = sprintf("file://%s", filename))
}
getElem.URISource <-
function(x)
{
    list(content = readContent(x$uri[x$position], x$encoding, x$mode),
         uri = x$uri[x$position])
}
getElem.VectorSource <-
function(x)
    list(content = x$content[x$position],
         uri = NULL)
getElem.XMLSource <-
function(x)
    list(content = XML::saveXML(x$content[[x$position]]),
         uri = x$uri)

pGetElem <- function(x) UseMethod("pGetElem", x)
pGetElem.DataframeSource <-
function(x)
    lapply(seq_len(x$length),
           function(y) list(content = x$content[y,],
                            uri = NULL))
pGetElem.DirSource <-
function(x)
    lapply(x$filelist,
           function(f) list(content = readContent(f, x$encoding, x$mode),
                            uri = sprintf("file://%s", f)))
pGetElem.URISource <-
function(x)
    lapply(x$uri,
           function(uri) list(content = readContent(uri, x$encoding, x$mode),
                              uri = uri))
pGetElem.VectorSource <-
function(x)
    lapply(x$content,
           function(y) list(content = y,
                            uri = NULL))

eoi <-
function(x)
    UseMethod("eoi", x)
eoi.SimpleSource <-
function(x)
    x$length <= x$position
