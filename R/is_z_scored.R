#' Test if a vector follows a standard normal distribution
#'
#' Mean should roughly zero and standard deviation one. Tolerance (tol)
#' should be too low. Even rnorm(100) may have a mean of 0.07 or so.
#' With tol = 0.01, is_z_score(rnorm(100)) would return FALSE.
#'
#' @param x vector
#' @param tol accepted deviation for mean and sd from 0 and 1, respectively
#' @param verbose print message if not z-scored?
#'
#' @return logical
#' @export
#'
#' @examples
#' is_z_scored(runif(100))
#' replicate(10, is_z_scored(rnorm(100)))
#' is_z_scored(scale(runif(100))[,1])
is_z_scored <- function(x,
                        tol = 0.1,
                        verbose = T) {
    if (!is.numeric(x)) {
        stop("x must be a numeric vector.")
    }
    if (!is.numeric(tol)) {
        stop("tol must be a numeric vector.")
    }
    if (tol < 0) {
        message("tol set to 0.")
        tol <- 0
    }
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    is_z <- (abs(m) < tol && (s - 1) < tol)
    if (!is_z && verbose) {
        message("mean: ", round(m, get_decimal_places(tol)+1), ", sd: ", round(s, get_decimal_places(tol)+1))
    }
    return(is_z)
}

get_decimal_places <- function(x) {
    if (!is.finite(x)) return(NA)  # Handle Inf, -Inf, NaN
    x_str <- sub("0+$", "", sub("^.*\\.", "", format(x, scientific = FALSE)))
    nchar(x_str)
}

