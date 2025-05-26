#' Scale vector of matrix from min to max value
#'
#' @param x numeric vector or matrix
#' @param min min value
#' @param max max value
#' @param margin margin if x is matrix
#'
#' @return numeric vector or matrix
#' @export
#'
#' @examples
#' scale_min_max(rnorm(100), -1, 1)
scale_min_max <- function (x, min = 0, max = 1, margin = 2) {
    if (is.matrix(x) || is.data.frame(x)) {
        if (is.data.frame(x)) {
            if (!all(apply(x, 2, is.numeric))) {
                stop("Please make sure that all columns of the data frame are numeric.")
            }
        }
        if (margin == 1) {
            return(do.call(rbind, apply(x, margin, function(y) min +
                                            ((y - min(y)) * (max - min)/(max(y) - min(y))),
                                        simplify = F)))
        }
        if (margin == 2) {
            return(do.call(cbind, apply(x, margin, function(y) min +
                                            ((y - min(y)) * (max - min)/(max(y) - min(y))),
                                        simplify = F)))
        }
    }
    else {
        return(min + ((x - min(x)) * (max - min)/(max(x) - min(x))))
    }
}
