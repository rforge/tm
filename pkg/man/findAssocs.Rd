\name{findAssocs}
\alias{findAssocs}
\alias{findAssocs.DocumentTermMatrix}
\alias{findAssocs.TermDocumentMatrix}
\alias{findAssocs.matrix}
\title{Find Associations in a Term-Document Matrix}
\description{
  Find associations in a term-document matrix.
}
\usage{
\method{findAssocs}{DocumentTermMatrix}(x, terms, corlimit)
\method{findAssocs}{TermDocumentMatrix}(x, terms, corlimit)
\method{findAssocs}{matrix}(x, terms, corlimit)
}
\arguments{
  \item{x}{A document-term matrix, a term-document matrix, or a dense
    correlation matrix.}
  \item{terms}{a character holding terms occuring in \code{x}.}
  \item{corlimit}{a numeric for the lower correlation limit in the range
    from zero to one.}
}
\value{A named numeric with those different terms from \code{x} which correlate
with \code{terms} more than \code{corlimit}.}   
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
findAssocs(tdm, c("oil", "opec"), 0.7)
}
