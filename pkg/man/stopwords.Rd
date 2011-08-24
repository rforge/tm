\name{stopwords}
\alias{stopwords}
\title{Multilingual Stopwords}
\description{
  Return stopwords in different languages.
}
\usage{
stopwords(language = "en")
}
\arguments{
  \item{language}{A \code{character} giving the desired language.}
}
\details{
  At the moment supported languages are \code{danish}, \code{dutch},
  \code{english}, \code{finnish}, \code{french}, \code{german},
  \code{hungarian}, \code{italian}, \code{norwegian}, \code{portuguese},
  \code{russian}, \code{spanish}, and \code{swedish}. Language names are
  case sensitive. Alternatively, their \acronym{IETF} language tags may
  be used.
}
\value{A character vector containing the requested stopwords. An error
  is raised if no stopwords are available for the requested
  \code{language}.}
\examples{
stopwords("en")
}
\keyword{file}
