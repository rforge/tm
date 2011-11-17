# Map IETF language tags to languages used by the Snowball stemmer
# http://en.wikipedia.org/wiki/IETF_language_tag
map_IETF_Snowball <- function(code) {
    codes <- c("da", "nl", "en", "fi", "fr", "de", "hu", "it", "no", "pt", "ru", "es", "sv")
    names <- c("danish", "dutch", "english", "finnish", "french", "german", "hungarian",
               "italian", "norwegian", "portuguese", "russian", "spanish", "swedish")

    names[charmatch(gsub("-.*", "", code), codes)]
}

stopwords <- {
    function(kind = "en") {
        resolved <- tm:::map_IETF_Snowball(kind)
        base <- if (is.na(resolved))
            kind
        else
            resolved
        s <- system.file("stopwords", paste(base, ".dat", sep = ""), package = "tm")
        if (identical(s, ""))
            stop(paste("no stopwords available for '", base, "'", sep = ""))
        readLines(s, encoding = "UTF-8")
    }
}
