#' Interpolate a Numeric Vector
#'
#' Linearly interpolates a numeric vector using either a desired output length
#' or explicit x-coordinates.
#'
#' There are two supported modes:
#' \enumerate{
#'   \item Provide `y` and `len_out` to resample the vector to a specified
#'   number of equally spaced points.
#'   \item Provide `y` and `x` to interpolate at arbitrary x-values. If
#'   `x_out` is not supplied, interpolation is performed at unit intervals
#'   between `min(x)` and `max(x)`.
#' }
#'
#' Interpolation is performed using [stats::approx()].
#'
#' @param y A numeric vector containing the values to interpolate.
#' @param x Optional numeric vector of x-coordinates corresponding to `y`.
#'   Must have the same length as `y`. Ignored when `len_out` is supplied.
#' @param len_out Optional integer specifying the desired number of output
#'   points. When provided, `x` is ignored and the input is assumed to be
#'   equally spaced.
#' @param x_out Optional numeric vector of x-values at which to evaluate the
#'   interpolation. Only used when `x` is supplied. If `NULL`, a sequence with
#'   a step size of 1 from `min(x)` to `max(x)` is generated.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{x}{The x-values used for interpolation.}
#'   \item{y}{The interpolated values.}
#' }
#'
#' @seealso [stats::approx()]
#'
#' @export
#'
#' @examples
#' # Resample to a fixed number of points
#' interpolate_vec(y = 1:10, len_out = 20)
#'
#' # Interpolate irregularly spaced observations
#' x <- c(1, 2, 4, 7, 10)
#' y <- c(3, 5, 2, 8, 6)
#' interpolate_vec(y = y, x = x)
#'
#' # Interpolate at user-defined locations
#' interpolate_vec(
#'   y = y,
#'   x = x,
#'   x_out = seq(1, 10, by = 0.5)
#' )
interpolate_vec <- function(y,
                            x = NULL,
                            len_out = NULL,
                            x_out = NULL) {
    if (!is.null(len_out)) {
        if (!is.null(x)) {
            message("Ignoring 'x' because 'len_out' was supplied.")
        }
        len_in <- length(y)
        x_out <- seq(1, len_in, length.out = len_out)
        y <- stats::approx(1:len_in, y, xout = x_out)$y
    } else if (!is.null(x)) {
        if (is.null(x_out)) {
            message("x_out is NULL. Interpolating at unit intervals.")
            x_out <- seq(min(x), max(x), 1)
        }
        y <- stats::approx(x, y, xout = x_out)$y
    } else {
        message("Provide either (1) 'y' and 'len_out', or (2) 'y' and 'x' (optionally with 'x_out').")
    }
    return(data.frame(x = x_out, y = y))
}
