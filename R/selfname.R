#' Name a vector by its elements
#'
#' Concise self naming. No magic though.
#'
#' @param x vector
#'
#' @return named vector
#' @export
#'
#' @examples
#' selfname(letters)
selfname <- function(x) {
    stats::setNames(x,x)
}
