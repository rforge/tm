## Author: Ingo Feinerer
## Reader

getReaders <-
function()
    c("readDOC", "readPDF", "readReut21578XML", "readReut21578XMLasPlain",
      "readPlain", "readRCV1", "readRCV1asPlain", "readTabular", "readXML")

prepareReader <- function(readerControl, defaultReader = NULL, ...) {
    if (is.null(readerControl$reader))
        readerControl$reader <- defaultReader
    if (inherits(readerControl$reader, "FunctionGenerator"))
        readerControl$reader <- readerControl$reader(...)
    if (is.null(readerControl$language))
        readerControl$language <- "en"
    readerControl
}

readPlain <-
function(elem, language, id)
    PlainTextDocument(elem$content, id = id, language = language)

readXML <- FunctionGenerator(function(spec, doc) {
    stopifnot(is.list(spec), inherits(doc, "TextDocument"))

    spec <- spec
    doc <- doc
    function(elem, language, id) {
        tree <- XML::xmlParse(elem$content, asText = TRUE)
        content(doc) <- if ("Content" %in% names(spec))
            .xml_content(tree, spec[["Content"]])
        else
            XML::xmlTreeParse(elem$content, asText = TRUE)
        for (n in setdiff(names(spec), "Content"))
            meta(doc, n) <- .xml_content(tree, spec[[n]])
        XML::free(tree)
        if (!is.na(language))
            meta(doc, "Language") <- language
        doc
    }
})

Reut21578XMLSpec <-
    list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
         DateTimeStamp = list("function", function(node)
           strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"),
                           XML::xmlValue),
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
         Exchanges = list("node", "/REUTERS/EXCHANGES/D"))
readReut21578XML <- readXML(spec = Reut21578XMLSpec, doc = XMLTextDocument())
readReut21578XMLasPlain <-
readXML(spec = c(Reut21578XMLSpec,
                 list(Content = list("node", "/REUTERS/TEXT/BODY"))),
        doc = PlainTextDocument())

RCV1Spec <-
    list(Author = list("unevaluated", ""),
         DateTimeStamp = list("function", function(node)
           as.POSIXlt(as.character(XML::getNodeSet(node, "/newsitem/@date")),
                      tz = "GMT")),
         Description = list("unevaluated", ""),
         Heading = list("node", "/newsitem/title"),
         ID = list("attribute", "/newsitem/@itemid"),
         Origin = list("unevaluated", "Reuters Corpus Volume 1"),
         Publisher = list("attribute",
           "/newsitem/metadata/dc[@element='dc.publisher']/@value"),
         Topics = list("attribute",
           "/newsitem/metadata/codes[@class='bip:topics:1.0']/code/@code"),
         Industries = list("attribute",
           "/newsitem/metadata/codes[@class='bip:industries:1.0']/code/@code"),
         Countries = list("attribute",
           "/newsitem/metadata/codes[@class='bip:countries:1.0']/code/@code"))
readRCV1 <- readXML(spec = RCV1Spec, doc = XMLTextDocument())
readRCV1asPlain <-
readXML(spec = c(Reut21578XMLSpec,
                 list(Content = list("node", "/newsitem/text"))),
        doc = PlainTextDocument())

# readDOC needs antiword installed to be able to extract the text
readDOC <- FunctionGenerator(function(AntiwordOptions = "") {
    stopifnot(is.character(AntiwordOptions))

    AntiwordOptions <- AntiwordOptions
    function(elem, language, id) {
        content <- system2("antiword",
                           c(AntiwordOptions, shQuote(elem$uri)),
                           stdout = TRUE)
        PlainTextDocument(content, id = id, language = language)
    }
})

readPDF <-
FunctionGenerator(function(engine = c("xpdf", "Rpoppler", "ghostscript",
                                      "Rcampdf", "custom"),
                           control = list(info = NULL, text = NULL))
{
    stopifnot(is.character(engine), is.list(control))

    engine <- match.arg(engine)
    control <- control

    pdf_info <-
        switch(engine,
               xpdf = function(x) pdf_info_via_xpdf(x, control$info),
               Rpoppler = Rpoppler::PDF_info,
               ghostscript = pdf_info_via_gs,
               Rcampdf = Rcampdf::pdf_info,
               custom = control$info)

    pdf_text <-
        switch(engine,
               xpdf = function(x) system2("pdftotext",
                                          c(control$text, shQuote(x), "-"),
                                          stdout = TRUE),
               Rpoppler = Rpoppler::PDF_text,
               ghostscript = pdf_text_via_gs,
               Rcampdf = Rcampdf::pdf_text,
               custom = control$text)

    if (!is.function(pdf_info) || !is.function(pdf_text))
        stop("invalid function for PDF extraction")

    function(elem, language, id) {
        meta <- pdf_info(elem$uri)
        content <- pdf_text(elem$uri)
        PlainTextDocument(content, meta$Author, meta$CreationDate, meta$Subject,
                          meta$Title, id, meta$Creator, language)
     }
})

readTabular <- FunctionGenerator(function(mapping) {
    mapping <- mapping
    function(elem, language, id) {
        doc <- PlainTextDocument(
          content = as.character(elem$content[, mapping[["Content"]]]),
          id = id, language = language)
        for (n in setdiff(names(mapping), "Content"))
            meta(doc, n) <- elem$content[, mapping[[n]]]
        doc
    }
})
