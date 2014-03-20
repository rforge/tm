\name{URISource}
\alias{URISource}
\title{Uniform Resource Identifier Source}
\description{
  Constructs a source which represents documents located by a uniform resource
  identifier.
}
\usage{
URISource(x, encoding = "unknown", mode = "text")
}
\arguments{
  \item{x}{A vector of Uniform Resource Identifiers.}
  \item{encoding}{encoding to be assumed for input strings. It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{URISource} which extends the class
  \code{Source} representing documents located by a \acronym{URI}.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{DirSource}} for accessing a directory, and
  \code{\link{getSources}} to list available sources.
  \code{\link{Encoding}} on encodings in \R.
}
\examples{
loremipsum <- system.file("texts", "loremipsum.txt", package = "tm")
ovid <- system.file("texts", "txt", "ovid_1.txt", package = "tm")
us <- URISource(sprintf("file://\%s", c(loremipsum, ovid)))
inspect(Corpus(us))
}
