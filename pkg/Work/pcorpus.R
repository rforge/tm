library("tm")
acq <- system.file("texts", "acq", package = "tm")
tmpfile <- tempfile()
r <- PCorpus(DirSource(acq),
             readerControl = list(reader = readReut21578XMLasPlain),
             dbControl = list(dbName = tmpfile, dbType = "DB1"))
print(r)
inspect(r[2:3])
r <- tm_map(r, stripWhitespace)
r <- tm_map(r, content_transformer(tolower))
r <- tm_map(r, removeWords, stopwords("english"))
r <- tm_map(r, stemDocument)

r[meta(r, "id") == '10' &
  meta(r, "heading") == 'COMPUTER TERMINAL SYSTEMS <CPML> COMPLETES SALE']
meta(r, tag = "test", "corpus") <- 1:3
meta(r, tag = "cl1") <- 1:10
print(r)
meta(r, type = "corpus")
meta(r)
meta(r, tag = "test", "corpus") <- NULL
meta(r, tag = "cl1") <- NULL
meta(r, type = "corpus")
meta(r)
tdm <- TermDocumentMatrix(r)
inspect(tdm[50:55, 1:8])

rm(r)
file.remove(tmpfile)
