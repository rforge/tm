\name{XMLTextDocument}
\alias{XMLTextDocument}
\title{XML Text Documents}
\description{
  Create \acronym{XML} text documents.
}
\usage{
XMLTextDocument(x = list(),
                author = character(0),
                datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                description = character(0),
                heading = character(0),
                id = character(0),
                origin = character(0),
                language = character(0),
                \dots)
}
\arguments{
  \item{x}{An \code{"\link[XML:xmlTreeParse]{XMLDocument}"}.}
  \item{author}{a character giving the author names.}
  \item{datetimestamp}{an object of class \code{"POSIXlt"} giving the creation
    date and time.}
  \item{description}{a character giving a description.}
  \item{heading}{a character giving the title or a short heading.}
  \item{id}{a character giving a unique identifier.}
  \item{origin}{a character giving information on the source and origin.}
  \item{language}{a character giving the language (preferably as \acronym{IETF}
    language tags).}
  \item{\dots}{user-defined document metadata tag-value pairs.}
}
\value{
  An object inheriting from \code{"XMLTextDocument"} and
  \code{"\link{TextDocument}"}.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{tm}.
}
\examples{
xml <- system.file("exampleData", "test.xml", package = "XML")
(xtd <- XMLTextDocument(XML::xmlTreeParse(xml),
                        heading = "XML text document",
                        id = xml,
                        language = "en"))
meta(xtd)
as.character(xtd)
}
\author{Ingo Feinerer}
