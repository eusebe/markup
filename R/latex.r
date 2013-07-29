##' Emphasis
##'
##' @param x x
##' @param style style
##' @param where where
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords internal
emphase.latex <- function(x, style, where) {
    close <- "}"
    if (style == "strong") open <- "\\textbf{"
    if (style == "italic") open <- "\\textit{"
    open.mat <- expand(open, nrow(x), ncol(x))
    open.mat[!where] <- ""
    close.mat <- expand(close, nrow(x), ncol(x))
    close.mat[!where] <- ""
    enclose(x, open.mat, close.mat)
}

##' Header
##'
##' @param x x
##' @param caption caption
##' @param align align
##' @param include.rownames include.rownames
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords internal
header.latex <- function(x, caption = NULL, align = "r", include.rownames = FALSE) {
    j <- paste("|", paste(rep(align, length = ncol(x)), collapse = "|"), "|", sep = "")
    if (include.rownames) {
        j <- paste("|l", j, sep = "")
    }
    cat("\\begin{table}[htbp]\n")
    cat("\\centering\n")
    if (!is.null(caption)) {
        cat("\\caption{", caption, "}\n", sep = "")
    }
    cat("\\begin{tabular}{", j, "}\n", sep = "")
    cat("\\hline\n")
}

##' footer
##'
##' @author David Hajage \email{dhajage@@gmail.com}
##' @keywords internal
footer.latex <- function() {
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{table}\n")
}

tbgroups.latex <- function(groups, n.groups, shift = 0, add = 0) {
    paste(paste(rep(" & ", shift), collapse = ""), paste("\\multicolumn{", n.groups, "}{|c|}{\\textbf{", groups, "}}", sep = "", collapse = " & "), paste(rep(" & ", add), collapse = ""), " \\\\", sep = "")
}

##' .. content for \description{} (no empty lines) ..
##'
##'
##' @param x
##' @param format
##' @param digits
##' @param decimal.mark
##' @param drop0trailing
##' @param na.print
##' @param include.rownames
##' @param include.colnames
##' @param rownames
##' @param colnames
##' @param align
##' @param strong
##' @param italic
##' @param caption
##' @param ... ...
##' @return
##' @author David Hajage \email{dhajage@@gmail.com}
tab.latex <- function(x, format = "f", digits = 2, decimal.mark = ".", drop0trailing = FALSE, na.print = "", include.rownames = FALSE, include.colnames = FALSE, rownames = NULL, colnames = NULL, align = "r", strong = NULL, italic = NULL, caption = NULL, tgroup = NULL, n.tgroup = NULL, bgroup = NULL, n.bgroup = NULL, ...) {
    charx <- tocharac(x, format = format, digits = digits, decimal.mark = decimal.mark, drop0trailing = drop0trailing, na.print = na.print, include.rownames = include.rownames, include.colnames = include.colnames, rownames = rownames, colnames = colnames)

    if (!is.null(strong)) {
        if (include.rownames) strong <- cbind(FALSE, strong)
        if (include.colnames) strong <- rbind(FALSE, strong)
        charx <- emphase.latex(charx, "strong", strong)
    }
    if (!is.null(italic)) {
        if (include.rownames) italic <- cbind(FALSE, italic)
        if (include.colnames) italic <- rbind(FALSE, italic)
        charx <- emphase.latex(charx, "italic", italic)
    }

    if (include.colnames) {
        charx[1, ] <- emphase.latex(charx[1, ], "strong", TRUE)
    }
    header.latex(x, caption = caption, align = align, include.rownames = include.rownames)
    res <- paste(paste.matrix(charx, collapse = " & "), "\\\\")
    if (include.colnames) {
        res <- c(res[1], "\\hline", res[-1])
    }
    if (!is.null(tgroup)) {
        if (!is.list(tgroup)) tgroup <- list(tgroup)
        if (!is.list(n.tgroup)) n.tgroup <- list(n.tgroup)
        n.tgroup <- mapply(sizegroups, tgroup, n.tgroup, MoreArgs = list(ncol(x)), SIMPLIFY = FALSE)
        tgroups <- mapply(tbgroups.latex, tgroup, n.tgroup, MoreArgs = list(shift = include.rownames))
        res <- c(paste(rev(tgroups), "\n\\hline", sep = ""), res)
    }
    if (!is.null(bgroup)) {
        if (!is.list(bgroup)) bgroup <- list(bgroup)
        if (!is.list(n.bgroup)) n.bgroup <- list(n.bgroup)
        n.bgroup <- mapply(sizegroups, bgroup, n.bgroup, MoreArgs = list(ncol(x)), SIMPLIFY = FALSE)
        bgroups <- mapply(tbgroups.latex, bgroup, n.bgroup, MoreArgs = list(shift = include.rownames))
        res <- c(res, paste("\\hline\n", bgroups, sep = ""))
    }
    cat(res, sep = "\n")
    footer.latex()
}
