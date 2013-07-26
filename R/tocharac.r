##' format p values
##'
##' @param p p values
##' @param digits number of digits
##' @param drop0trailing see \code{?formatC}
##' @return A character vector (formated p values)
##' @export
##' @author David Hajage
plim <- function(p, digits = 2, drop0trailing = FALSE) {
  pround <- round(p, digits)
  lim <- 10^(-digits)
  ptxt <- vector("character", length(p))
  ptxt[pround < lim] <- paste("<", "0.", paste(rep("0", digits-1), collapse = ""), "1", sep = "")
  ptxt[pround >= lim] <- formatC(pround[pround >= lim], format = "f", digits = digits, drop0trailing = drop0trailing)
  return(ptxt)
}

##' Transform to character
##'
##' Coerce a vector, a matrix or a data.frame into character
##'
##' @param x a vector, a matrix, or a data.frame
##' @param format see \code{?formatC}. Same options, plus one: \code{"p"} (see \code{?plim})
##' @param digits see \code{?formatC}
##' @param drop0trailing see \code{?formatC}
##' @param include.rownames if \code{TRUE}, results will include row names
##' @param include.colnames if \code{TRUE}, results will include column names
##' @param ... not used
##' @aliases tocharac tocharac.default tocharac.matrix tocharac.data.frame
##' @return A character vector or matrix
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords manip
##' @rdname tocharac
##' @export
##' @examples
##' tocharac(1:4)
##' tocharac(head(esoph))
tocharac <- function (x, ...) {
    UseMethod("tocharac")
}

##' @method tocharac default
##' @rdname tocharac
##' @export
tocharac.default <- function(x, format = "f", digits = 2, drop0trailing = FALSE, ...) {

    if (is.numeric(x)) {
        if (format != "p") {
            char <- formatC(x, digits = digits, format = format, drop0trailing = drop0trailing)
        } else {
            char <- plim(x, digits = digits, drop0trailing = drop0trailing)
        }
    } else {
        char <- as.character(x)
    }
    char
}

##' @method tocharac matrix
##' @rdname tocharac
##' @export
tocharac.matrix <- function(x, format = "f", digits = 2, drop0trailing = FALSE, include.rownames = FALSE, include.colnames = FALSE, ...) {
    format <- expand(format, nrow(x), ncol(x), drop = FALSE)
    digits <- expand(digits, nrow(x), ncol(x), drop = FALSE)
    drop0trailing <- expand(drop0trailing, nrow(x), ncol(x), drop = FALSE)

    res <- matrix(nrow = nrow(x), ncol = ncol(x))
    for (i in 1:nrow(x)) {
        for (j in 1:ncol(x)) {
            res[i, j] <- tocharac(x[i, j], format = format[i, j], digits = digits[i, j], drop0trailing = drop0trailing[i, j])
        }
    }

    rn <- rownames(x)
    if (is.null(rn)) rn <- rep("", nrow(x))
    cn <- colnames(x)
    if (is.null(cn)) cn <- rep("", ncol(x))
    if (include.rownames & !include.colnames) {
        res <- cbind(rn, res)
    }
    if (!include.rownames & include.colnames) {
        res <- rbind(cn, res)
    }
    if (include.rownames & include.colnames) {
        res <- cbind(c("", rn), rbind(cn, res))
    }
    dimnames(res) <- NULL

    res

}

##' @method tocharac data.frame
##' @rdname tocharac
##' @export
tocharac.data.frame <- function(x, format = "f", digits = 2, drop0trailing = FALSE, include.rownames = FALSE, include.colnames = FALSE, ...) {
    res <- tocharac.matrix(x, format = format, digits = digits, drop0trailing = drop0trailing, include.rownames = include.rownames, include.colnames = include.colnames)
    res
}

##' Justify strings
##'
##' Justify strings
##'
##' @param x a vector, a matrix, or a data.frame
##' @param side \code{"left"}, \code{"right"} or \code{"centre"}. Could be a vector or a matrix.
##' @param add the minimum number of \code{" "} character to add. Default is \code{2}
##' @param ... not used
##' @aliases justify justify.default justify.matrix justify.data.frame
##' @return A character vector or matrix. All columns will have the same length of strings.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords manip
##' @rdname justify
##' @import stringr
##' @export
##' @examples
##' justify(tocharac(head(esoph)))
justify <- function (x, ...) {
    UseMethod("justify")
}

##' @method justify default
##' @rdname justify
##' @export
justify.default <- function(x, side = "left", add = 2, ...) {
    max_width <- max(str_length(x)) + add
    side <- rep(side, length = length(x))

    s <- side
    s[side == "centre"] <- "both"
    s[side == "left"] <- "right"
    s[side == "right"] <- "left"

    res <- vector("character", length(x))
    for (i in 1:length(x)) {
        res[i] <- str_pad(x[i], max_width, side = s[i])
    }
    res
}

##' @method justify matrix
##' @rdname justify
##' @export
justify.matrix <- function(x, side = "left", add = 2, ...) {
    side <- expand(side, nrow(x), ncol(x), drop = FALSE)

    res <- matrix(nrow = nrow(x), ncol = ncol(x))
    for (i in 1:ncol(x)) {
        res[, i] <- justify(x[, i], side = side[, i], add = add)
    }

    res
}

##' @method justify data.frame
##' @rdname justify
##' @export
justify.data.frame <- function(x, side = "left", add = 2, ...) {
    res <- justify.matrix(x, side = side, add = add)
    res
}
