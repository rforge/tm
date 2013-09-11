\name{tm_term_score}
\alias{tm_term_score}
\alias{tm_term_score.DocumentTermMatrix}
\alias{tm_term_score.term_frequency}
\alias{tm_term_score.PlainTextDocument}
\alias{tm_term_score.TermDocumentMatrix}
\title{Compute Score for Matching Terms}
\description{
  Compute a score based on the number of matching terms.
}
\usage{
\method{tm_term_score}{DocumentTermMatrix}(x, terms, FUN = slam::row_sums)
\method{tm_term_score}{PlainTextDocument}(x, terms, FUN = function(x) sum(x, na.rm = TRUE))
\method{tm_term_score}{term_frequency}(x, terms, FUN = function(x) sum(x, na.rm = TRUE))
\method{tm_term_score}{TermDocumentMatrix}(x, terms, FUN = slam::col_sums)
}
\arguments{
  \item{x}{Either a \code{\link{PlainTextDocument}}, a term frequency as
    returned by \code{\link{termFreq}}, or a
    \code{\link{TermDocumentMatrix}}.}
  \item{terms}{A character vector of terms to be matched.}
  \item{FUN}{A function computing a score from the number of terms
    matching in \code{x}.}
}
\value{
  A score as computed by \code{FUN} from the number of matching
  \code{terms} in \code{x}.
}
\examples{
data("acq")
tm_term_score(acq[[1]], c("company", "change"))
\dontrun{
## Test for positive and negative sentiments
## install.packages("tm.plugin.tags", repos = "http://datacube.wu.ac.at", type = "source")
require("tm.plugin.tags")
sapply(acq[1:10], tm_term_score, tm_get_tags("Positiv"))
sapply(acq[1:10], tm_term_score, tm_get_tags("Negativ"))
tm_term_score(TermDocumentMatrix(acq[1:10],
                                control = list(removePunctuation = TRUE)),
             tm_get_tags("Positiv"))
}
}
