##' Size of each group
##'
##' @param g groups (a character vector)
##' @param n total number
##' @param ng optionnaly, the size of each group (corrected if \code{sum(ng) != n}
##' @return the size of each group, with the constraint that \code{sum(ng) = n}
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords internal
sizegroups <- function(g, ng = NULL, n) {
    if (is.null(ng)) {
        ng <- rep(round(n/length(g)), length(g))
    } else {
        ng <- rep(ng, length = length(g))
    }

    if (sum(ng) != n) {
        ng[length(ng)] <- ng[length(ng)] + n - sum(ng)
    }

    if (any(ng <= 0)) {
        ng <- groups(g, NULL, n)
    }
    ng
}

