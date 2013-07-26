\name{tocharac}
\alias{tocharac}
\alias{tocharac.data.frame}
\alias{tocharac.default}
\alias{tocharac.matrix}
\title{Transform to character}
\usage{
  tocharac(x, ...)

  \method{tocharac}{default} (x, format = "f", digits = 2,
    decimal.mark = ".", drop0trailing = FALSE,
    na.print = "", ...)

  \method{tocharac}{matrix} (x, format = "f", digits = 2,
    decimal.mark = ".", drop0trailing = FALSE,
    na.print = "", include.rownames = FALSE,
    include.colnames = FALSE, rownames = NULL,
    colnames = NULL, ...)

  \method{tocharac}{data.frame} (x, format = "f",
    digits = 2, decimal.mark = ".", drop0trailing = FALSE,
    na.print = "", include.rownames = FALSE,
    include.colnames = FALSE, rownames = NULL,
    colnames = NULL, ...)
}
\arguments{
  \item{x}{a vector, a matrix, or a data.frame}

  \item{format}{see \code{?formatC}. Same options, plus
  one: \code{"p"} (see \code{?plim})}

  \item{digits}{see \code{?formatC}}

  \item{decimal.mark}{see \code{?formatC}}

  \item{drop0trailing}{see \code{?formatC}}

  \item{na.print}{character string specifying how \code{NA}
  should be formatted specially}

  \item{include.rownames}{if \code{TRUE}, results will
  include row names}

  \item{include.colnames}{if \code{TRUE}, results will
  include column names}

  \item{rownames}{character vector (replicated or truncated
  as necessary) indicating rownames of the corresponding
  rows. If \code{NULL} (default) rownames are not modified}

  \item{colnames}{character vector (replicated or truncated
  as necessary) indicating colnames of the corresponding
  columns. If \code{NULL} (default) colnames are not
  modified}

  \item{...}{not used}
}
\value{
  A character vector or matrix
}
\description{
  Coerce a vector, a matrix or a data.frame into character
}
\examples{
tocharac(1:4)
tocharac(head(esoph))
}
\author{
  David Hajage \email{dhajage@gmail.com}
}
\keyword{manip}

