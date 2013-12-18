\name{findAssocs}
\alias{findAssocs}
\alias{findAssocs.DocumentTermMatrix}
\alias{findAssocs.TermDocumentMatrix}
\title{Find Associations in a Term-Document Matrix}
\description{
  Find associations in a term-document or document-term matrix.
}
\usage{
\method{findAssocs}{DocumentTermMatrix}(x, terms, corlimit)
\method{findAssocs}{TermDocumentMatrix}(x, terms, corlimit)
}
\arguments{
  \item{x}{A \code{\link{DocumentTermMatrix}} or a
    \code{\link{TermDocumentMatrix}}.}
  \item{terms}{a character holding terms occuring in \code{x}.}
  \item{corlimit}{a numeric vector (of the same length as \code{terms}) for the
    lower correlation limits of each term in the range from zero to one.}
}
\value{A named numeric with those different terms from \code{x} which correlate
with \code{terms} more than \code{corlimit}.}   
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
findAssocs(tdm, c("oil", "opec"), c(0.7, 0.75))
}
