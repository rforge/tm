\name{termFreq}
\alias{termFreq}
\title{Term Frequency Vector}
\description{
  Generate a term frequency vector from a text document.
}
\usage{
termFreq(doc, control = list())
}
\arguments{
  \item{doc}{An object inheriting from \code{TextDocument}.}
  \item{control}{A list of control options which override default
    settings. Possible options are
    \describe{
      \item{\code{tolower}}{A function converting characters to lower
	case. Defaults to \code{\link{tolower}}.}
      \item{\code{removePunctuation}}{A logical value indicating whether
	punctuation characters should be removed from
	\code{doc}, or a function which performs punctuation
	removal. Defaults to \code{FALSE}.}
      \item{\code{tokenize}}{A function tokenizing documents into single
	tokens or a string matching one of the predefined tokenization
	functions:
	\describe{
	  \item{\code{scan}}{for \code{\link{scan_tokenizer}}, or}
	  \item{\code{MC}}{for \code{\link{MC_tokenizer}}.}
	}
	Defaults to \code{\link{scan_tokenizer}}.}
      \item{\code{removeNumbers}}{A logical value indicating whether
	numbers should be removed from \code{doc}. Defaults to \code{FALSE}.}
      \item{\code{stemming}}{Either a Boolean value indicating whether tokens
	should be stemmed or a stemming function. Defaults to \code{FALSE}.}
      \item{\code{stopwords}}{Either a Boolean value indicating stopword
	removal using default language specific stopword lists shipped
	with this package or a character vector holding custom
	stopwords. Defaults to \code{FALSE}.}
      \item{\code{dictionary}}{A character vector to be tabulated
	against. No other terms will be listed in the result. Defaults
	to \code{NULL} which means that all terms in \code{doc} are
	listed.}
      \item{\code{minDocFreq}}{An integer value. Words that appear less
	often in \code{doc} than this number are discarded. Defaults to
	\code{1} (i.e., every token will be used).}
      \item{\code{minWordLength}}{An integer value. Words smaller than
	this number are discarded. Defaults to length \code{3}.}
    }
  }
}
\value{
  A named integer vector of class \code{term_frequency} with term
  frequencies as values and tokens as names.
}
\seealso{
  \code{\link{getTokenizers}}
}
\examples{
data("crude")
termFreq(crude[[1]])
strsplit_space_tokenizer <- function(x) unlist(strsplit(x, "[[:space:]]+"))
ctrl <- list(removePunctuation = TRUE, tokenize = strsplit_space_tokenizer,
             stemming = TRUE, minWordLength = 4)
termFreq(crude[[1]], control = ctrl)
}
\keyword{math}
