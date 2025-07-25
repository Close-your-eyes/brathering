#' Interpolate a vector (time series)
#'
#' Either provide y and len_out or y and x with x_out optional.
#'
#' @param y numeric vector of y values
#' @param len_out output length
#' @param x optionally x values to y
#' @param x_out sequence of x to interpolate at, if NULL step of 1 between
#' min(x) and max(x) are chosen
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' interpolate_vec(y = seq(1,10), len_out = 20)
interpolate_vec <- function(y,
                            x = NULL,
                            len_out = NULL,
                            x_out = NULL) {
    if (!is.null(len_out)) {
        if (!is.null(x)) {
            message("len_out provided. ignoring x.")
        }
        len_in <- length(y)
        x_out <- seq(1, len_in, length.out = len_out)
        y <- stats::approx(1:len_in, y, xout = x_out)$y
    } else if (!is.null(x)) {
        if (is.null(x_out)) {
            message("x_out is NULL. interpoating to steps of 1.")
            x_out <- seq(min(x), max(x), 1)
        }
        y <- stats::approx(x, y, xout = x_out)$y
    } else {
        message("Either provide (i) y and len_out or (ii) y and x with x_out optionally.")
    }
    return(data.frame(x = x_out, y = y))
}
