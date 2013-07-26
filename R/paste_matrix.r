##' Expand a matrix
##'
##' \code{expand} expand a matrix to a new \code{nrow} and \code{ncol} matrix.
##'
##' Values are replicated as necessary to fit the new dimensions of the matrix, or replaced by \code{what}.
##'
##' @param x A matrix
##' @param nrow Number of rows
##' @param ncol Number of columns
##' @param what If \code{nrow > nrow(x)} and/or \code{ncol > ncol(x)}, values are replaced by what's in \code{what}
##' @param drop If \code{TRUE}, delete the unnecessary dimension of a matrix (with \code{\link{drop}})
##' @return A vector or a matrix.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords manip
##' @export
##' @examples
##' mat <- matrix(1:4, 2, 2)
##' expand(mat, 6, 7)
##' expand(mat, 6, 7, what = NA)
##' expand(mat, 1, 7)
##' expand(mat, 1, 7, drop = FALSE)
expand <- function(x, nrow = NULL, ncol = NULL, what = NULL, drop = TRUE) {
    ## If x is a vector, transormation into a matrix with one row and length(x) columns
    if (is.vector(x))
        x <- t(x)

    if (is.null(nrow)) {
        nrow = nrow(x)
    }
    if (is.null(ncol)) {
        ncol = ncol(x)
    }

    if (nrow == 0 | ncol == 0) {
        ## return a matrix with 0 rows and/or 0 columns
        xx <- matrix(nrow = nrow, ncol = ncol)
    } else if (nrow(x) == 0 | ncol(x) == 0) {
        if (!is.null(what)) {
            xx <- matrix(what, nrow, ncol)
        } else {
            xx <- matrix(nrow = nrow, ncol = ncol)
        }
    } else if (nrow(x) > nrow & ncol(x) > ncol) {
        xx <- x[1:nrow, 1:ncol]
    } else if (!is.null(what)) {
        minnrow <- min(c(nrow(x), nrow))
        minncol <- min(c(ncol(x), ncol))
        xx <- matrix(nrow = nrow, ncol = ncol)
        xx[1:minnrow, 1:minncol] <- x[1:minnrow, 1:minncol]
        xx[is.na(xx)] <- what
    } else {
        if (nrow(x) > 1 & ncol(x) == 1 & ncol == 1) {
            xx <- apply(x, 2, rep, length = nrow)
        } else {
            xx <- apply(t(apply(x, 1, rep, length = ncol)), 2, rep, length = nrow)
        }
    }
    if (!drop) {
        dim(xx) <- c(nrow, ncol)
    } else {
        xx <- drop(xx)
    }
    xx
}
##' Paste for matrix
##'
##' \code{paste.matrix} is a generalization of \code{\link{paste}} for matrix.
##'
##' @param ... One or more matrix. All matrix are expand to the maximum dimensions with \code{\link{expand}}.
##' @param sep a character string to separate the terms.  Not \code{NA}.
##' @param collapse an optional character string to separate the results.  Not \code{NA}.
##' @param byrow if \code{!is.null{collapse}} and \code{byrow = TRUE}, collapsing will be done by rows.
##' @return A character matrix.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords manip
##' @export
##' @examples
##' mat <- matrix(1:4, 2, 2)
##' cat(paste("|", sep = "", paste.matrix(mat, "|", sep = "", collapse = "")), sep = "\n")
paste.matrix <- function(..., sep = " ", collapse = NULL, byrow = FALSE) {
  args <- list(...)
  args <- args[!sapply(args, is.null)]

  args <- lapply(args, function(x) {
    if (is.vector(x))
      t(x)
    else
      as.matrix(x)
  })

  ncol.max <- max(sapply(args, function(x) ncol(x)))
  nrow.max <- max(sapply(args, function(x) nrow(x)))

  mats <- lapply(args, function(x) {
    x <- expand(x, nrow.max, ncol.max)
    x
  })

  results <- matrix(do.call("paste", c(mats, sep = sep)), nrow.max, ncol.max)
  if (!is.null(collapse)) {
    margin <- 1
    if (byrow)
      margin <- 2
    results <- apply(results, margin, paste, collapse = collapse)
  }
  results
}
