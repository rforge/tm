# readOOO needs unoconv (which in turn needs OpenOffice) installed
readOOO <- FunctionGenerator(function(unoconvOptions = "", ...) {
    unoconvOptions <- unoconvOptions
    function(elem, language, id) {
        tmp <- tempfile()
        # Unfortunately unoconv does not have an output file option and writes the output to the same directory as the input
        # In addition conversion to stdout may corrupt the zip file (odt) if writing it out via writeLines()
        if (!all(file.copy(elem$uri, sprintf("%s.oo", tmp))))
            stop(sprintf("cannot copy %s", elem$uri))
        system(paste("unoconv -f odt", sprintf("%s.oo", tmp)))
        meta.xml <- unzip(sprintf("%s.odt", tmp), "meta.xml", exdir = dirname(tmp))[1]

        on.exit(file.remove(sprintf("%s.oo", tmp), sprintf("%s.odt", tmp), meta.xml))

	tree <- XML::xmlParse(meta.xml)
        root <- XML::xmlRoot(tree)

        content <- system(paste("unoconv -f txt --stdout", shQuote(elem$uri)), intern = TRUE)
        author <- XML::xpathSApply(root, "/office:document-meta/office:meta/dc:creator", XML::xmlValue)
        datetimestamp <- as.POSIXlt(XML::xpathSApply(root, "/office:document-meta/office:meta/dc:date", XML::xmlValue))

        XML::free(tree)

        PlainTextDocument(content, author, datetimestamp, id = id, language = language)
    }
})
