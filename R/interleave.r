# from http://tolstoy.newcastle.edu.au/R/help/06/03/22717.html

##' Interleave
##'
##' These functions interleave the elements of several lists or several vectors or several matrix or several data frames.
##' @param ... lists, vectors, matrix or data.frames (all the same classes).
##' @param byrow for matrix and data.frames, interleaving will be done by rows (\code{TRUE}) or by columns (\code{FALSE})
##' @param pretty.rownames for data.frames, when \code{byrow = TRUE}, rownames of the output will use the name of the objects in \code{...}
##' @aliases interleave interleave.default interleave.matrix interleave.data.frame
##' @return This function returns an object of the same class of what's in \code{...}
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords manip
##' @rdname interleave
##' @export
##' @examples
interleave <- function (...) {
    UseMethod("interleave")
}

##' @method interleave default
##' @export
interleave.default <- function(...) {
  args <- list(...)
  args <- args[!sapply(args, is.null)]
  nargs <- length(args)

  ord <- c()
  for (i in 1:nargs) {
    ord <- c(ord, nargs*(1:length(args[[i]])) - nargs + i)
  }
  do.call("c", args)[order(ord)]
}
##' Transform a matrix to a list
##'
##' @param x a matrix
##' @param byrow each element of the list will correspond to each row (\code{byrow = TRUE}) or each column (\code{byrow = FALSE}) of the matrix
##' @param ...
##' @return a list
##' @export
##' @method as.list matrix
##' @keywords list
##' @author David Hajage \email{dhajage@gmail.com}
as.list.matrix <- function(x, byrow = TRUE) {
  margin <- 2
  if (byrow)
    margin <- 1

  lapply(apply(x, margin, list), function(x) x[[1]])
}

##' @method interleave matrix
##' @export
interleave.matrix <- function(..., byrow = TRUE) {
  args <- list(...)
  args <- args[!sapply(args, is.null)]

  lists <- lapply(args, function(x) {
    as.list.matrix(x, byrow = byrow)
  })
  interlists <- do.call("interleave.default", lists)
  if (byrow)
    do.call("rbind", interlists)
  else
    do.call("cbind", interlists)
}

##' @method interleave data.frame
##' @export
interleave.data.frame <- function(..., byrow = TRUE, pretty.rownames = TRUE) {
  args <- list(...)
  args <- args[!sapply(args, is.null)]

  names_df <- lapply(args, names)
  if (byrow) {
    inter_names <- names(args[[1]])
    class_df <- lapply(args[[1]], class)
    if (pretty.rownames) {
      args_names <- names(args)
      real_args_names <- as.character(as.list(substitute(list(...)))[-1])
      if (is.null(args_names))
        args_names <- real_args_names
      args_names[args_names == ""] <- real_args_names[args_names == ""]
      for (i in 1:length(args)) {
        row.names(args[[i]]) <- paste(args_names[i], row.names(args[[i]]), sep = " ")
      }
    }
  } else {
    inter_names <- unlist(do.call("interleave.default", names_df))
  }

  list_mat <- lapply(args, as.matrix)
  names(list_mat) <- NULL
  results <- suppressWarnings(data.frame(do.call("interleave.matrix", c(list_mat, byrow = byrow))))
  names(results) <- inter_names
  if (byrow) {
    for (i in 1:ncol(results)) {
      class(results[, i]) <- class_df[[i]]
    }
  }
  results
}
