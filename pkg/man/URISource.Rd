\name{URISource}
\alias{URISource}
\title{Uniform Resource Identifier Source}
\description{
  Constructs a source which represents documents located by a uniform resource
  identifier.
}
\usage{
URISource(x, encoding = "", mode = "text")
}
\arguments{
  \item{x}{A vector of Uniform Resource Identifiers.}
  \item{encoding}{A character string describing the current encoding. It is
    passed to \code{\link{iconv}} to convert the input to \code{"UTF-8"}.}
}
\value{
  An object of class \code{URISource} which extends the class
  \code{Source} representing documents located by a \acronym{URI}.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{DirSource}} for accessing a directory, and
  \code{\link{getSources}} to list available sources.
  \code{\link{Encoding}} and \code{\link{iconv}} on encodings in \R.
}
\examples{
loremipsum <- system.file("texts", "loremipsum.txt", package = "tm")
ovid <- system.file("texts", "txt", "ovid_1.txt", package = "tm")
us <- URISource(sprintf("file://\%s", c(loremipsum, ovid)))
inspect(Corpus(us))
}
