## readOOO needs unoconv installed
## http://dag.wieers.com/home-made/unoconv/
readOOO <-
function(elem, language, id)
{
    uri <- normalizePath(tm:::normalizeURI(elem$uri))

    fodt <- system2("unoconv",
                    c("-f fodt --stdout", shQuote(uri)),
                    stdout = TRUE)
    tree <- XML::xmlParse(fodt, asText = TRUE)
    root <- XML::xmlRoot(tree)
    author <- XML::xpathSApply(root,
                               "/office:document/office:meta/dc:creator",
                               XML::xmlValue)
    datetimestamp <-
        as.POSIXlt(XML::xpathSApply(root,
                                    "/office:document/office:meta/dc:date",
                                    XML::xmlValue))
    XML::free(tree)

    content <- system2("unoconv",
                      c("-f txt --stdout", shQuote(uri)),
                      stdout = TRUE)

    PlainTextDocument(content, author, datetimestamp, id = id,
                      language = language)
}
