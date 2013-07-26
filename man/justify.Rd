\name{justify}
\alias{justify}
\alias{justify.data.frame}
\alias{justify.default}
\alias{justify.matrix}
\title{Justify strings}
\usage{
  justify(x, ...)

  \method{justify}{default} (x, side = "left", add = 2,
    ...)

  \method{justify}{matrix} (x, side = "left", add = 2, ...)

  \method{justify}{data.frame} (x, side = "left", add = 2,
    ...)
}
\arguments{
  \item{x}{a vector, a matrix, or a data.frame}

  \item{side}{\code{"left"}, \code{"right"} or
  \code{"centre"}. Could be a vector or a matrix.}

  \item{add}{the minimum number of \code{" "} character to
  add. Default is \code{2}}

  \item{...}{not used}
}
\value{
  A character vector or matrix. All columns will have the
  same string length.
}
\description{
  Justify strings
}
\examples{
justify(tocharac(head(esoph)))
}
\author{
  David Hajage \email{dhajage@gmail.com}
}
\keyword{manip}

