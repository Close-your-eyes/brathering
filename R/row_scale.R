#' Title
#'
#' @param x
#' @param center
#' @param scale
#' @param add_attr
#' @param rows
#' @param cols
#' @param drop.na.rows
#'
#' @return
#' @export
#'
#' @examples
row_scale = function(x,
                     center = TRUE,
                     scale = TRUE,
                     add_attr = TRUE,
                     rows = NULL,
                     cols = NULL,
                     drop.na.rows = T) {

    ## redo this in c++
    ## sparse mat!

    if (!is.null(rows) && !is.null(cols)) {
        x <- x[rows, cols, drop = FALSE]
    } else if (!is.null(rows)) {
        x <- x[rows, , drop = FALSE]
    } else if (!is.null(cols)) {
        x <- x[, cols, drop = FALSE]
    }

    rm = Matrix::rowMeans(x, na.rm = TRUE)
    if (scale) {
        csd = apply(x, 1, stats::sd)
        #csd = matrixStats::rowSds(x, center = rm)
    } else {
        csd = rep(1, length = length(rm))
    }
    if (!center) {
        rm = rep(0, length = length(rm))
    }
    x = (x - rm) / csd

    if (drop.na.rows) {
        x <- x[which(Matrix::rowSums(is.na(x))<ncol(x)),,drop=F]
    }

    if (add_attr) {
        if (center) {
            attr(x, "scaled:center") <- rm
        }
        if (scale) {
            attr(x, "scaled:scale") <- csd
        }
    }
    return(x)
}
