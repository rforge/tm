# Author: Ingo Feinerer

PCorpus <-
function(x,
         readerControl = list(reader = x$defaultreader, language = "en"),
         dbControl = list(dbName = "", dbType = "DB1"))
{
    stopifnot(inherits(x, "Source"))

    readerControl <- prepareReader(readerControl, x$defaultreader)

    if (is.function(readerControl$init))
        readerControl$init()

    if (is.function(readerControl$exit))
        on.exit(readerControl$exit())

    if (!filehash::dbCreate(dbControl$dbName, dbControl$dbType))
        stop("error in creating database")
    db <- filehash::dbInit(dbControl$dbName, dbControl$dbType)

    # Allocate memory in advance if length is known
    tdl <- if (x$length > 0) vector("list", as.integer(x$length)) else list()

    counter <- 1
    while (!eoi(x)) {
        x <- stepNext(x)
        elem <- getElem(x)
        id <- if (is.null(x$names) || is.na(x$names))
            as.character(counter)
        else
            x$names[counter]
        doc <- readerControl$reader(elem, readerControl$language, id)
        filehash::dbInsert(db, meta(doc, "id"), doc)
        if (x$length > 0) tdl[[counter]] <- meta(doc, "id")
        else tdl <- c(tdl, meta(doc, "id"))
        counter <- counter + 1
    }
    if (!is.null(x$names) && !is.na(x$names))
        names(tdl) <- x$names

    structure(list(content = tdl,
                   meta = CorpusMeta(),
                   dmeta = data.frame(row.names = seq_along(tdl)),
                   dbcontrol = dbControl),
              class = c("PCorpus", "Corpus"))
}

VCorpus <- Corpus <-
function(x, readerControl = list(reader = x$defaultreader, language = "en"))
{
    stopifnot(inherits(x, "Source"))

    readerControl <- prepareReader(readerControl, x$defaultreader)

    if (is.function(readerControl$init))
        readerControl$init()

    if (is.function(readerControl$exit))
        on.exit(readerControl$exit())

    # Allocate memory in advance if length is known
    tdl <- if (x$length > 0) vector("list", as.integer(x$length)) else list()

    if (x$vectorized)
        tdl <- mapply(function(elem, id)
                          readerControl$reader(elem, readerControl$language, id),
                      pGetElem(x),
                      id = if (is.null(x$names) || is.na(x$names))
                          as.character(seq_len(x$length))
                      else x$names,
                      SIMPLIFY = FALSE)
    else {
        counter <- 1
        while (!eoi(x)) {
            x <- stepNext(x)
            elem <- getElem(x)
            id <- if (is.null(x$names) || is.na(x$names))
                as.character(counter)
            else
                x$names[counter]
            doc <- readerControl$reader(elem, readerControl$language, id)
            if (x$length > 0)
                tdl[[counter]] <- doc
            else
                tdl <- c(tdl, list(doc))
            counter <- counter + 1
        }
    }
    if (!is.null(x$names) && !is.na(x$names))
        names(tdl) <- x$names

    structure(list(content = tdl,
                   meta = CorpusMeta(),
                   dmeta = data.frame(row.names = seq_along(tdl))),
              class = c("VCorpus", "Corpus"))
}

`[.PCorpus` <-
function(x, i)
{
    if (!missing(i)) {
        x$content <- x$content[i]
        x$dmeta <- x$dmeta[i, , drop = FALSE]
    }
    x
}

`[.VCorpus` <-
function(x, i)
{
    if (!missing(i)) {
        x$content <- x$content[i]
        x$dmeta <- x$dmeta[i, , drop = FALSE]
        if (!is.null(x$lazy))
            x$lazy$index <- x$lazy$index[i]
    }
    x
}

.map_name_index <-
function(x, i)
{
    if (is.character(i))
        match(i, if (is.null(names(x))) meta(x, "id", "local") else names(x))
    else
        i
}

`[[.PCorpus` <-
function(x, i)
{
    i <- .map_name_index(x, i)
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    filehash::dbFetch(db, x$content[[i]])
}
`[[.VCorpus` <-
function(x, i)
{
    i <- .map_name_index(x, i)
    if (!is.null(x$lazy))
        .Call(copyCorpus, x, materialize(x, i))
    x$content[[i]]
}

`[[<-.PCorpus` <-
function(x, i, value)
{
    i <- .map_name_index(x, i)
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    db[[x$content[[i]]]] <- value
    x
}
`[[<-.VCorpus` <-
function(x, i, value)
{
    i <- .map_name_index(x, i)
    # Mark new objects as inactive for lazy mapping
    if (!is.null(x$lazy))
        x$lazy$index[i] <- FALSE
    x$content[[i]] <- value
    x
}

#c2 <-
#function(x, y, ...)
#{
#    # Update the CMetaData tree
#    cmeta <- .MetaDataNode(0, list(merge_date = as.POSIXlt(Sys.time(), tz = "GMT"), merger = Sys.getenv("LOGNAME")), list(CMetaData(x), CMetaData(y)))
#    update.struct <- .update_id(cmeta)
#
#    new <- .VCorpus(c(unclass(x), unclass(y)), update.struct$root, NULL)
#
#    # Find indices to be updated for the left tree
#    indices.mapping <- .find_indices(x)
#
#    # Update the CorpusDMeta data frames for the left tree
#    for (i in 1:ncol(update.struct$left.mapping)) {
#        map <- update.struct$left.mapping[,i]
#        DMetaData(x)$MetaID <- replace(DMetaData(x)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
#    }
#
#    # Find indices to be updated for the right tree
#    indices.mapping <- .find_indices(y)
#
#    # Update the CorpusDMeta data frames for the right tree
#    for (i in 1:ncol(update.struct$right.mapping)) {
#        map <- update.struct$right.mapping[,i]
#        DMetaData(y)$MetaID <- replace(DMetaData(y)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
#    }
#
#    # Merge the CorpusDMeta data frames
#    labels <- setdiff(names(DMetaData(y)), names(DMetaData(x)))
#    na.matrix <- matrix(NA,
#                        nrow = nrow(DMetaData(x)),
#                        ncol = length(labels),
#                        dimnames = list(row.names(DMetaData(x)), labels))
#    x.dmeta.aug <- cbind(DMetaData(x), na.matrix)
#    labels <- setdiff(names(DMetaData(x)), names(DMetaData(y)))
#    na.matrix <- matrix(NA,
#                        nrow = nrow(DMetaData(y)),
#                        ncol = length(labels),
#                        dimnames = list(row.names(DMetaData(y)), labels))
#    y.dmeta.aug <- cbind(DMetaData(y), na.matrix)
#    DMetaData(new) <- rbind(x.dmeta.aug, y.dmeta.aug)
#
#    new
#}

c.VCorpus <-
function(..., recursive = FALSE)
{
    args <- list(...)
    x <- args[[1L]]

    if (length(args) == 1L)
        return(x)

    if (!all(unlist(lapply(args, inherits, class(x)))))
        stop("not all arguments are of the same corpus type")

    if (recursive)
        Reduce(c2, args)
    else {
        args <- do.call("c", lapply(args, content))
        structure(list(content = args,
                       meta = CorpusMeta(),
                       dmeta = data.frame(row.names = seq_along(args))),
                  class = c("VCorpus", "Corpus"))
    }
}

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

as.list.PCorpus <- as.list.VCorpus <-
function(x, ...)
    content(x)

content.VCorpus <-
function(x)
{
    if (!is.null(x$lazy))
        .Call(copyCorpus, x, materialize(x))
    x$content
}

content.PCorpus <-
function(x)
{
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    filehash::dbMultiFetch(db, unlist(x$content))
}

length.PCorpus <- length.VCorpus <-
function(x)
    length(x$content)

print.PCorpus <- print.VCorpus <-
function(x, ...)
{
    cat(sprintf(ngettext(length(x),
                         "A corpus with %d text document\n\n",
                         "A corpus with %d text documents\n\n"),
                length(x)))

    meta <- meta(x, type = "corpus")
    dmeta <- meta(x, type = "indexed")

    cat("Metadata:\n")
    cat(sprintf("  Tag-value pairs. Tags: %s\n",
                paste(names(meta), collapse = " ")))
    cat("  Data frame. Variables:", colnames(dmeta), "\n")

    invisible(x)
}

inspect <-
function(x)
    UseMethod("inspect", x)
inspect.PCorpus <- inspect.VCorpus <-
function(x)
{
    print(x)
    cat("\n")
    print(noquote(content(x)))
    invisible(x)
}

writeCorpus <-
function(x, path = ".", filenames = NULL)
{
    filenames <- file.path(path,
      if (is.null(filenames))
          sprintf("%s.txt", as.character(meta(x, "id", "local")))
      else filenames)

    stopifnot(length(x) == length(filenames))

    mapply(function(doc, f) writeLines(as.character(doc), f), x, filenames)

    invisible(x)
}
