\name{interleave}
\alias{interleave}
\alias{interleave.data.frame}
\alias{interleave.default}
\alias{interleave.matrix}
\title{Interleave}
\usage{
  interleave(...)

  \method{interleave}{default} (...)

  \method{interleave}{matrix} (..., byrow = TRUE)

  \method{interleave}{data.frame} (..., byrow = TRUE,
    pretty.rownames = TRUE)
}
\arguments{
  \item{...}{lists, vectors, matrix or data.frames (but all
  the same classes).}

  \item{byrow}{for matrix and data.frames, interleaving
  will be done by rows (\code{TRUE}) or by columns
  (\code{FALSE})}

  \item{pretty.rownames}{for data.frames, when \code{byrow
  = TRUE}, rownames of the output will use the name of the
  objects in \code{...}}
}
\value{
  This function returns an object of the same class of
  what's in \code{...}
}
\description{
  These functions interleave the elements of several lists
  or several vectors or several matrix or several data
  frames.
}
\examples{
interleave(1:4, 5:15)
interleave(matrix(1:4, 2, 2), matrix(5:8, 2, 2))
interleave(matrix(1:4, 2, 2), matrix(5:8, 2, 2), byrow = FALSE)
interleave(head(esoph), tail(esoph))
}
\author{
  David Hajage \email{dhajage@gmail.com}
}
\keyword{manip}

