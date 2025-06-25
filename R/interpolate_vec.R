#' Title
#'
#' @param x
#' @param len_out
#'
#' @return
#' @export
#'
#' @examples
interpolate_vec <- function(x, len_out) {
    len_in <- length(x)

    # Create a new index sequence for interpolation
    new_index <- seq(1, len_in, length.out = len_out)

    # Interpolate
    x <- approx(1:len_in, x, xout = new_index)$y
    return(x)
}
