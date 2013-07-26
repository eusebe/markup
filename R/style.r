##' Enclose character strings between \code{open} and \code{close}
##'
##' Enclose character strings between \code{open} and \code{close}. \code{x} could be a vector and or matrix. \code{open} and \code{close} are expanded to fit the dimension of \code{x}.
##' @param x A character vector matrix
##' @param open Opening character vector or matrix
##' @param close Closing character vector or matrix
##' @return A vector or a matrix, with the same dimensions.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords manip
##' @export
##' @examples
##' enclose(head(esoph))
##' enclose(head(esoph), open = c("*", "+"), close = c(" ", "!"))
enclose <- function(x, open = "*", close = open) {
    open <- expand(open, nrow(x), ncol(x), drop = FALSE)
    close <- expand(close, nrow(x), ncol(x), drop = FALSE)

    res <- paste.matrix(open, x, close, sep = "")
    res[res %in% unique(paste(open, close, sep = ""))] <- ""
    dim(res) <- dim(x)
    res
}
