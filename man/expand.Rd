\name{expand}
\alias{expand}
\title{Expand a matrix}
\usage{
  expand(x, nrow = NULL, ncol = NULL, what = NULL,
    drop = TRUE)
}
\arguments{
  \item{x}{A matrix}

  \item{nrow}{Number of rows}

  \item{ncol}{Number of columns}

  \item{what}{If \code{nrow > nrow(x)} and/or \code{ncol >
  ncol(x)}, values are replaced by what's in \code{what}}

  \item{drop}{If \code{TRUE}, delete the unnecessary
  dimension of a matrix (with \code{\link{drop}})}
}
\value{
  A vector or a matrix.
}
\description{
  \code{expand} expand a matrix to a new \code{nrow} and
  \code{ncol} matrix.
}
\details{
  Values are replicated as necessary to fit the new
  dimensions of the matrix, or replaced by \code{what}.
}
\examples{
mat <- matrix(1:4, 2, 2)
expand(mat, 6, 7)
expand(mat, 6, 7, what = NA)
expand(mat, 1, 7)
expand(mat, 1, 7, drop = FALSE)
}
\author{
  David Hajage \email{dhajage@gmail.com}
}
\keyword{manip}

