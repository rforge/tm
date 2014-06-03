\name{PlainTextDocument}
\alias{PlainTextDocument}
\title{Plain Text Documents}
\description{
  Create plain text documents.
}
\usage{
PlainTextDocument(x = character(0),
                  author = character(0),
                  datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                  description = character(0),
                  heading = character(0),
                  id = character(0),
                  language = character(0),
                  origin = character(0),
                  \dots,
                  meta = NULL,
                  class = NULL)
}
\arguments{
  \item{x}{A character giving the plain text content.}
  \item{author}{a character or an object of class \code{\link{person}} giving
    the author names.}
  \item{datetimestamp}{an object of class \code{\link{POSIXt}} or a character
    in \acronym{ISO} 8601 format giving the creation date and time.}
  \item{description}{a character giving a description.}
  \item{heading}{a character giving the title or a short heading.}
  \item{id}{a character giving a unique identifier.}
  \item{language}{a character giving the language (preferably as \acronym{IETF}
    language tags).}
  \item{origin}{a character giving information on the source and origin.}
  \item{\dots}{user-defined document metadata tag-value pairs.}
  \item{meta}{a named list or \code{NULL} (default) giving all metadata. If set
    all other metadata arguments are ignored.}
  \item{class}{a character vector or \code{NULL} (default) giving
    additional classes to be used for the created plain text document.}
}
\value{
  An object inheriting from \code{class}, \code{PlainTextDocument} and
  \code{\link{TextDocument}}.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{tm}.
}
\references{
  \acronym{ISO} 8601 Date and Time Formats.
  \url{http://www.w3.org/TR/NOTE-datetime}
}
\examples{
(ptd <- PlainTextDocument("A simple plain text document",
                          heading = "Plain text document",
                          id = basename(tempfile()),
                          language = "en"))
meta(ptd)
}
