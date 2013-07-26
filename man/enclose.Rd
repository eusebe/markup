\name{enclose}
\alias{enclose}
\title{Enclose character strings between \code{open} and \code{close}}
\usage{
  enclose(x, open = "*", close = open)
}
\arguments{
  \item{x}{A character vector matrix}

  \item{open}{Opening character vector or matrix}

  \item{close}{Closing character vector or matrix}
}
\value{
  A vector or a matrix, with the same dimensions.
}
\description{
  Enclose character strings between \code{open} and
  \code{close}. \code{x} could be a vector and or matrix.
  \code{open} and \code{close} are expanded to fit the
  dimension of \code{x}.
}
\examples{
enclose(head(esoph))
enclose(head(esoph), open = c("*", "+"), close = c(" ", "!"))
}
\author{
  David Hajage \email{dhajage@gmail.com}
}
\keyword{manip}

