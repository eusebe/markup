\name{paste.matrix}
\alias{paste.matrix}
\title{Paste for matrix}
\usage{
  paste.matrix(..., sep = " ", collapse = NULL,
    byrow = FALSE)
}
\arguments{
  \item{...}{One or more matrix. All matrix are expand to
  the maximum dimensions with \code{\link{expand}}.}

  \item{sep}{a character string to separate the terms.  Not
  \code{NA}.}

  \item{collapse}{an optional character string to separate
  the results.  Not \code{NA}.}

  \item{byrow}{if \code{!is.null{collapse}} and \code{byrow
  = TRUE}, collapsing will be done by rows.}
}
\value{
  A character matrix.
}
\description{
  \code{paste.matrix} is a generalization of
  \code{\link{paste}} for matrix.
}
\examples{
mat <- matrix(1:4, 2, 2)
cat(paste("|", sep = "", paste.matrix(mat, "|", sep = "", collapse = "")), sep = "\\n")
}
\author{
  David Hajage \email{dhajage@gmail.com}
}
\keyword{manip}

