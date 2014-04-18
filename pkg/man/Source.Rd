\name{Source}
\alias{Source}
\alias{SimpleSource}
\alias{eoi}
\alias{eoi.SimpleSource}
\alias{getElem}
\alias{getElem.DataframeSource}
\alias{getElem.DirSource}
\alias{getElem.URISource}
\alias{getElem.VectorSource}
\alias{getElem.XMLSource}
\alias{getSources}
\alias{pGetElem}
\alias{pGetElem.DataframeSource}
\alias{pGetElem.DirSource}
\alias{pGetElem.URISource}
\alias{pGetElem.VectorSource}
\alias{Source}
\alias{stepNext}
\alias{stepNext.SimpleSource}
\title{Sources}
\description{
  Creating and accessing sources.
}
\usage{
SimpleSource(defaultReader = readPlain,
             encoding = "unknown",
             length = NA_integer_,
             names = NA_character_,
             position = 0,
             vectorized = TRUE,
             \dots,
             class)
getSources()
\method{eoi}{SimpleSource}(x)
\method{getElem}{DataframeSource}(x)
\method{getElem}{DirSource}(x)
\method{getElem}{URISource}(x)
\method{getElem}{VectorSource}(x)
\method{getElem}{XMLSource}(x)
\method{pGetElem}{DataframeSource}(x)
\method{pGetElem}{DirSource}(x)
\method{pGetElem}{URISource}(x)
\method{pGetElem}{VectorSource}(x)
\method{stepNext}{SimpleSource}(x)
}
\arguments{
  \item{x}{A \code{Source}.}
  \item{defaultReader}{a reader function (generator).}
  \item{encoding}{a character giving the encoding of the elements delivered by
    the source.}
  \item{length}{an integer denoting the number of elements delivered by the
    source. If the length is unknown in advance it must be set to
    \code{NA_integer_}.}
  \item{names}{a character vector giving element names.}
  \item{position}{a numeric indicating the current position in the source.}
  \item{vectorized}{a logical indicating the ability for parallel element
    access.}
  \item{\dots}{tag-value pairs for storing additional information.}
  \item{class}{a character vector giving additional classes to be used for the
    created source.}
}
\details{
  \emph{Sources} abstract input locations, like a directory, a connection, or
  simply an \R vector, in order to acquire content in a uniform way. In packages
  which employ the infrastructure provided by package \pkg{tm}, such sources are
  represented via the virtual S3 class \code{Source}: such packages then provide
  S3 source classes extending the virtual base class (such as
  \code{\link{DirSource}} provided by package \pkg{tm} itself).

  All extension classes must provide implementations for the interface functions
  \code{eoi}, \code{getElem}, and \code{stepNext}. Individual components must be
  accessible via \code{$}. If \code{x$vectorized} is \code{TRUE} the function
  \code{pGetElem} must be provided as well.

  The function \code{eoi} indicates end of input. \code{getElem} fetches the
  element at the current position, whereas \code{pGetElem} retrieves all
  elements in parallel at once. \code{stepNext} increases the position in the
  source to acquire the next element.

  The function \code{SimpleSource} provides a simple implementation and should
  be used when creating custom sources.
}
\value{
  For \code{SimpleSource()}, an object inheriting from \code{class},
  \code{SimpleSource}, and \code{Source}.

  For \code{getSources()}, a character vector with sources provided by package
  \pkg{tm}.

  For \code{eoi()}, a logical indicating if the end of input of the source is
  reached.

  For \code{getElem()} and \code{pGetElem()} a named list with the components
  \code{content} holding the document and \code{uri} giving a uniform resource
  identifier (e.g., a file path or \acronym{URL}; \code{NA} if not applicable or
  unavailable).
}
\seealso{
  \code{\link{DataframeSource}}, \code{\link{DirSource}},
  \code{\link{ReutersSource}}, \code{\link{URISource}}, and
  \code{\link{VectorSource}}.
}
