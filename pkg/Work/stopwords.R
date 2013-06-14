Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

snowball_stopword_files <-
    Sys.glob("~/src/org/tartarus/snowball/website/algorithms/*/stop.txt")
snowball_stopword_langs <-
    basename(dirname(snowball_stopword_files))
snowball_encodings <-
    rep.int("ISO-8859-1", length(snowball_stopword_langs))
names(snowball_encodings) <- snowball_stopword_langs
snowball_encodings["hungarian"] <- "ISO-8859-2"
snowball_encodings["russian"] <- "KOI8-R"

snowball_stopwords <-
    Map(function(f, lang) {
        con <- file(f, encoding = snowball_encodings[lang])
        on.exit(close(con))
        s <- readLines(con)
        s <- s[grepl("^[^[:space:][:punct:]]", s)]
        sub("[[:space:]].*", "", s)
    },
        snowball_stopword_files,
        snowball_stopword_langs)
names(snowball_stopwords) <- snowball_stopword_langs

tm_stopword_dir <-
    normalizePath("~/src/org/R-project/R-Forge/tm/pkg/inst/stopwords")

for(i in seq_along(snowball_stopwords))
    writeLines(snowball_stopwords[[i]],
               file.path(tm_stopword_dir,
                         sprintf("%s.dat",
                                 snowball_stopword_langs[i])))
