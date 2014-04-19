\name{tm_map}
\alias{tm_map}
\alias{tm_map.VCorpus}
\alias{tm_map.PCorpus}
\title{Transformations on Corpora}
\description{
  Interface to apply transformation functions (also denoted as mappings)
  to corpora.
}
\usage{
\method{tm_map}{PCorpus}(x, FUN, \dots)
\method{tm_map}{VCorpus}(x, FUN, \dots, lazy = FALSE)
}
\arguments{
  \item{x}{A corpus.}
  \item{FUN}{a transformation function taking a text document as input and
    returning a text document.}
  \item{\dots}{arguments to \code{FUN}.}
  \item{lazy}{a logical. Lazy mappings are mappings which are delayed
    until the content is accessed. It is useful for large corpora if only few
    documents will be accessed. In such a case it avoids the computationally
    expensive application of the mapping to all elements in the corpus.}
}
\value{
  A corpus with \code{FUN} applied to each document in \code{x}. In case
  of lazy mappings only internal flags are set. Access of individual documents
  triggers the execution of the corresponding transformation function.
}
\seealso{
  \code{\link{getTransformations}} for available transformations.
}
\note{
Lazy transformations change \R's standard evaluation semantics.
}
\examples{
data("crude")
tm_map(crude, stemDocument, lazy = TRUE)
## Generate a custom transformation function which takes the heading as new content
headings <- function(x)
    PlainTextDocument(meta(x, "heading"),
                      id = meta(x, "id"),
                      language = meta(x, "language"))
inspect(tm_map(crude, headings))
}
