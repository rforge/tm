\name{URISource}
\alias{URISource}
\title{Uniform Resource Identifier Source}
\description{
  Create a uniform resource identifier source.
}
\usage{
URISource(x, encoding = "", mode = "text")
}
\arguments{
  \item{x}{A character vector of uniform resource identifiers (\acronym{URI}s.}
  \item{encoding}{A character string describing the current encoding. It is
    passed to \code{\link{iconv}} to convert the input to \code{"UTF-8"}.}
}
\details{
  A \emph{uniform resource identifier source} interprets each \acronym{URI} as a
  document.
}
\value{
  An object inheriting from \code{URISource}, \code{\link{SimpleSource}},
  and \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}.

  \code{\link{Encoding}} and \code{\link{iconv}} on encodings.
}
\examples{
loremipsum <- system.file("texts", "loremipsum.txt", package = "tm")
ovid <- system.file("texts", "txt", "ovid_1.txt", package = "tm")
us <- URISource(sprintf("file://\%s", c(loremipsum, ovid)))
inspect(VCorpus(us))
}
