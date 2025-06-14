#' 2D contour estimation
#'
#' @param x matrix or x values, when matrix: col1 becomes x, col2 becomes y
#' @param y optional: y values when x is not a matrix
#' @param n number of bins, passed to MASS::kde2d
#' @param levels passed to grDevices::contourLines, if NULL determined by
#' algorithm
#' @param ... arguments to MASS::kde2d
#'
#' @return data frame
#' @export
#'
#' @examples
#' contour_est(data.frame(x = rnorm(1000), y = rnorm(1000)))
contour_est <- function(x,
                        y,
                        n = 100,
                        levels = NULL,
                        ...) {
    if (missing(y) && (is.matrix(x) || is.data.frame(x))) {
        name1 <- names(x)[1]
        name2 <- names(x)[2]
        y <- x[,2]
        x <- x[,1]
    } else {
        name1 <- "x"
        name2 <- "y"
    }
    dens <- MASS::kde2d(x = x, y = y, n = n, ...)
    if (is.null(levels)) {
        levels <- pretty(range(dens$z), n = 50)
    }
    paths <- purrr::map_dfr(grDevices::contourLines(dens$x, dens$y, dens$z, levels = levels), as.data.frame)
    names(paths)[c(2,3)] <- c(name1, name2)
    paths$level_norm <- brathering::scale2(x = paths$level)
    return(paths)
}
