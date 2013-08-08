## Author: Ingo Feinerer
## Reader

getReaders <- function()
    c("readDOC", "readGmane", "readPDF", "readReut21578XML", "readReut21578XMLasPlain", "readPlain", "readRCV1", "readRCV1asPlain", "readTabular", "readXML")

prepareReader <- function(readerControl, defaultReader = NULL, ...) {
    if (is.null(readerControl$reader))
        readerControl$reader <- defaultReader
    if (inherits(readerControl$reader, "FunctionGenerator"))
        readerControl$reader <- readerControl$reader(...)
    if (is.null(readerControl$language))
        readerControl$language <- "en"
    readerControl
}

readPlain <- FunctionGenerator(function(...) {
    function(elem, language, id) PlainTextDocument(elem$content, id = id, language = language)
})

readXML <- FunctionGenerator(function(spec, doc, ...) {
    spec <- spec
    doc <- doc
    function(elem, language, id) {
        tree <- XML::xmlParse(elem$content, asText = TRUE)
        Content(doc) <- if ("Content" %in% names(spec))
            .xml_content(tree, spec[["Content"]])
        else
            XML::xmlTreeParse(elem$content, asText = TRUE)
        for (n in setdiff(names(spec), "Content"))
            meta(doc, n) <- .xml_content(tree, spec[[n]])
        XML::free(tree)
        if (!is.na(language))
            attr(doc, "Language") <- language
        doc
    }
})

readGmane <- readXML(spec = list(Author = list("node", "/item/creator"),
                     Content = list("node", "/item/description"),
                     DateTimeStamp = list("function", function(node)
                     strptime(sapply(XML::getNodeSet(node, "/item/date"), XML::xmlValue),
                              format = "%Y-%m-%dT%H:%M:%S",
                              tz = "GMT")),
                     Description = list("unevaluated", ""),
                     Heading = list("node", "/item/title"),
                     ID = list("node", "/item/link"),
                     Origin = list("unevaluated", "Gmane Mailing List Archive")),
                     doc = PlainTextDocument())

readReut21578XML <- readXML(spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
                            DateTimeStamp = list("function", function(node)
                            strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                                     format = "%d-%B-%Y %H:%M:%S",
                                     tz = "GMT")),
                            Description = list("unevaluated", ""),
                            Heading = list("node", "/REUTERS/TEXT/TITLE"),
                            ID = list("attribute", "/REUTERS/@NEWID"),
                            TOPICS = list("attribute", "/REUTERS/@TOPICS"),
                            LEWISSPLIT = list("attribute", "/REUTERS/@LEWISSPLIT"),
                            CGISPLIT = list("attribute", "/REUTERS/@CGISPLIT"),
                            OLDID = list("attribute", "/REUTERS/@OLDID"),
                            Origin = list("unevaluated", "Reuters-21578 XML"),
                            Topics = list("node", "/REUTERS/TOPICS/D"),
                            Places = list("node", "/REUTERS/PLACES/D"),
                            People = list("node", "/REUTERS/PEOPLE/D"),
                            Orgs = list("node", "/REUTERS/ORGS/D"),
                            Exchanges = list("node", "/REUTERS/EXCHANGES/D")),
                            doc = Reuters21578Document())

readReut21578XMLasPlain <- readXML(spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
                                   Content = list("node", "/REUTERS/TEXT/BODY"),
                                   DateTimeStamp = list("function", function(node)
                                   strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                                            format = "%d-%B-%Y %H:%M:%S",
                                            tz = "GMT")),
                                   Description = list("unevaluated", ""),
                                   Heading = list("node", "/REUTERS/TEXT/TITLE"),
                                   ID = list("attribute", "/REUTERS/@NEWID"),
                                   TOPICS = list("attribute", "/REUTERS/@TOPICS"),
                                   LEWISSPLIT = list("attribute", "/REUTERS/@LEWISSPLIT"),
                                   CGISPLIT = list("attribute", "/REUTERS/@CGISPLIT"),
                                   OLDID = list("attribute", "/REUTERS/@OLDID"),
                                   Origin = list("unevaluated", "Reuters-21578 XML"),
                                   Topics = list("node", "/REUTERS/TOPICS/D"),
                                   Places = list("node", "/REUTERS/PLACES/D"),
                                   People = list("node", "/REUTERS/PEOPLE/D"),
                                   Orgs = list("node", "/REUTERS/ORGS/D"),
                                   Exchanges = list("node", "/REUTERS/EXCHANGES/D")),
                                   doc = PlainTextDocument())

readRCV1 <- readXML(spec = list(Author = list("unevaluated", ""),
                    DateTimeStamp = list("function", function(node)
                    as.POSIXlt(as.character(XML::getNodeSet(node, "/newsitem/@date")), tz = "GMT")),
                    Description = list("unevaluated", ""),
                    Heading = list("node", "/newsitem/title"),
                    ID = list("attribute", "/newsitem/@itemid"),
                    Origin = list("unevaluated", "Reuters Corpus Volume 1"),
                    Publisher = list("attribute", "/newsitem/metadata/dc[@element='dc.publisher']/@value"),
                    Topics = list("attribute", "/newsitem/metadata/codes[@class='bip:topics:1.0']/code/@code"),
                    Industries = list("attribute", "/newsitem/metadata/codes[@class='bip:industries:1.0']/code/@code"),
                    Countries = list("attribute", "/newsitem/metadata/codes[@class='bip:countries:1.0']/code/@code")),
                    doc = RCV1Document())

readRCV1asPlain <- readXML(spec = list(Author = list("unevaluated", ""),
                           Content = list("node", "/newsitem/text"),
                           DateTimeStamp = list("function", function(node)
                           as.POSIXlt(as.character(XML::getNodeSet(node, "/newsitem/@date")), tz = "GMT")),
                           Description = list("unevaluated", ""),
                           Heading = list("node", "/newsitem/title"),
                           ID = list("attribute", "/newsitem/@itemid"),
                           Origin = list("unevaluated", "Reuters Corpus Volume 1"),
                           Publisher = list("attribute", "/newsitem/metadata/dc[@element='dc.publisher']/@value"),
                           Topics = list("attribute", "/newsitem/metadata/codes[@class='bip:topics:1.0']/code/@code"),
                           Industries = list("attribute", "/newsitem/metadata/codes[@class='bip:industries:1.0']/code/@code"),
                           Countries = list("attribute", "/newsitem/metadata/codes[@class='bip:countries:1.0']/code/@code")),
                           doc = PlainTextDocument())

# readDOC needs antiword installed to be able to extract the text
readDOC <- FunctionGenerator(function(AntiwordOptions = "", ...) {
    AntiwordOptions <- AntiwordOptions
    function(elem, language, id) {
        content <- system2("antiword",
                           c(AntiwordOptions, shQuote(elem$uri)),
                           stdout = TRUE)
        PlainTextDocument(content, id = id, language = language)
    }
})

# readPDF needs pdfinfo and pdftotext installed to extract meta data and text
readPDF <- FunctionGenerator(function(PdftotextOptions = "", ...) {
    PdftotextOptions <- PdftotextOptions
    function(elem, language, id) {
        meta <- tm:::pdfinfo(elem$uri)
        content <- system2("pdftotext",
                           c(PdftotextOptions, shQuote(elem$uri), "-"),
                           stdout = TRUE)
        PlainTextDocument(content, meta$Author, meta$CreationDate, meta$Subject,
                          meta$Title, id, meta$Creator, language)
     }
})

readTabular <- FunctionGenerator(function(mapping, ...) {
    mapping <- mapping
    function(elem, language, id) {
        doc <- PlainTextDocument(id = id, language = language)
        for (n in names(mapping))
            content_meta(doc, n) <- elem$content[, mapping[[n]]]
        doc
    }
})

pdf_info_via_gs <-
function(file)
{
    file <- normalizePath(file)
    
    gs_cmd <- tools:::find_gs_cmd(Sys.getenv("R_GSCMD", ""))

    out <- system2(gs_cmd,
                   c("-dNODISPLAY -q",
                     sprintf("-sFile=%s", shQuote(file)),
                     system.file("ghostscript", "pdf_info.ps",
                                 package = "tm")),
                   stdout = TRUE)
    out <- out[cumsum(out == "") == 2L][-1L]
    val <- sub("^[^:]+:[[:space:]]*", "", out)
    names(val) <- sub(":.*", "", out)
    val <- as.list(val)
    if(!is.null(d <- val$CreationDate))
        val$CreationDate <- PDF_Date_to_POSIXt(d)
    if(!is.null(d <- val$ModDate))
        val$ModDate <- PDF_Date_to_POSIXt(d)

    val
}

PDF_Date_to_POSIXt <-
function(s)
{
    ## Strip optional 'D:' prefix.
    s <- sub("^D:", "", s)
    ## Strip apostrophes in offset spec.
    s <- gsub("'", "", s)
    if(nchar(s) <= 14L) {
        s <- sprintf("%s%s", s,
                     substring("    0101000000", nchar(s) + 1L, 14L))
        strptime(s, "%Y%m%d%H%M%S")
    } else if(substring(s, 15L, 15L) == "Z") {
        strptime(substring(s, 1L, 14L), "%Y%m%d%H%M%S")
    } else {
        strptime(s, "%Y%m%d%H%M%S%z")
    }
}

pdf_text_via_gs <-
function(file)
{
    files <- normalizePath(file)
    
    gs_cmd <- tools:::find_gs_cmd(Sys.getenv("R_GSCMD", ""))

    tf <- tempfile("pdf")
    on.exit(unlink(tf))

    ## The current mechanism is first converting PDF to Postscript using
    ## the ps2write device, and then extract text using the ps2ascii.ps
    ## program.  This fails for some files (e.g.,
    ## /data/rsync/PKGS/AlleleRetain/inst/doc/AlleleRetain_User_Guide.pdf
    ## which Ghostscript also fails to render.  Note that rendering via
    ## gv works "fine": but this uses the pswrite device which produces
    ## bitmap (from which no text can be extracted, of course).
    ## Using the txtwrite device is simply too unstable: e.g.,
    ##   gs -dBATCH -dNOPAUSE -sDEVICE=txtwrite -dQUIET -sOutputFile=- \
    ##     /data/rsync/PKGS/AlleleRetain/inst/doc/AlleleRetain_User_Guide.pdf
    ## keeps segfaulting.
    ## An additional nuisance is that there seems no simple way to
    ## detect a ps2ascii.ps failure.

    ## Finally, note that we currently use -DSIMPLE: without this, more
    ## information would be made available, but require post-processing.

    ## Step 1.  Convert PDF to Postscript.
    res <- system2(gs_cmd,
                   c("-q -dNOPAUSE -dBATCH -P- -dSAFER -sDEVICE=ps2write",
                     sprintf("-sOutputFile=%s", tf),
                     "-c save pop -f",
                     shQuote(file)))
    ## Step 2.  Extract text.
    txt <- system2(gs_cmd,
                   c("-q -dNODISPLAY -P- -dSAFER -dDELAYBIND -dWRITESYSTEMDICT -dSIMPLE",
                     "-c save -f ps2ascii.ps",
                     tf,
                     "-c quit"),
                   stdout = TRUE)
    ## Argh.  How can we catch errors?
    ## The return values are always 0 ...
    if(any(grepl("Error handled by opdfread.ps", txt))) {
        stop(paste(c("Ghostscript failed, with output:", txt),
                   collapse = "\n"))
    }

    strsplit(paste(txt, collapse = "\n"), "\f")[[1L]]
}
