#' Title
#'
#' compositions::clr(c(0.2,0.5,0.3,0))
#'
#' @param x numeric vector or matrix
#' @param margin if matrix: iterate over rows or cols?
#'
#' @returns vector or matrix
#' @export
#'
#' @examples
#' x <- c(NA,0.2,NA,0.3,0,0.5,0)
#' clr(x)
#' compositions::clr(x)
clr <- function(x, margin = 1) {

    clr_base <- function(x) {
        reconstruct <- function(x, zero_idx, original_length) {
            if (!length(zero_idx)) {
                return(x)
            }
            out <- numeric(original_length)
            out[-zero_idx] <- x
            out[zero_idx] <- 0
            return(out)
        }

        zero_idx <- which(x == 0)
        y <- x[x != 0]
        y <- brathering::na_rm(y)

        y[[1]] <- log(y[[1]] / brathering::geo_mean(y[[1]]))
        y <- brathering::na_insert(y[[1]], na_inds = y[[2]])

        x <- reconstruct(y, zero_idx, length(x))
        return(x)
    }

    if (!is.numeric(x)) {
        stop("x must be numeric.")
    }

    # if (any(dplyr::near(as.vector(x), 0))) {
    #     stop("zeros not handled yet.")
    # }

    if (is.vector(x)) {

        message("total: ", sum(x, na.rm = T))
        x <- clr_base(x)

    } else if (is.matrix(x) || is.data.frame(x)) {

        if (margin == 1 && unique(Matrix::rowSums(x, na.rm = T) != 1)) {
            stop("unequal rowsums.")
        }
        if (margin == 2 && unique(Matrix::colSums(x, na.rm = T) != 1)) {
            stop("unequal colsums.")
        }
        x <- apply(X = x, MARGIN = margin, clr_base)

    }
    return(x)
}


