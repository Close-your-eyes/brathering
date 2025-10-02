#' Scale vector of matrix from min to max value
#'
#' Checkout scales::rescale().
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
#' scale2(rnorm(100), -1, 1)
scale2 <- function (x, min = 0, max = 1, margin = 2) {

    if (is.matrix(x) || is.data.frame(x)) {
        if (is.data.frame(x)) {
            if (!all(apply(x, 2, is.numeric))) {
                stop("Please make sure that all columns of the data frame are numeric.")
            }
        }
        # if (margin == 1) {
        #     bindfun <- rbind
        # } else if (margin == 2) {
        #     bindfun <- cbind
        # } else {
        #     stop("margin must be 1 or 2 for iteration over rows or columns, respectively.")
        # }
        min1 <- apply(x, margin, min)
        max1 <- apply(x, margin, max)
        x <- apply(x, MARGIN = margin, scales::rescale, to = c(min, max))
        #y <- do.call(bindfun, apply(x, margin, scalefun, min = min, max = max, simplify = F))
    }
    else {
        min1 <- min(x)
        max1 <- max(x)
        #y <- scalefun(x, min, max)
        x <- scales::rescale(x, to = c(min, max))
    }
    attr(x, "min") <- min1
    attr(x, "max") <- max1
    return(x)
}

# scalefun <- function(x, min, max) {
#     if (dplyr::near(min(x), max(x))) {
#         return(x)
#     }
#     min + ((x - min(x)) * (max - min)/(max(x) - min(x)))
# }
