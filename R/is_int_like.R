#' Check if a numeric vector is integer-like
#'
#' Determines whether all elements in a numeric vector are effectively integers,
#' allowing for small floating-point inaccuracies.
#'
#' @param x A numeric vector to check.
#'
#' @returns A single logical value:
#' \describe{
#'   \item{TRUE}{if all elements of `x` are integer-like}
#'   \item{FALSE}{otherwise}
#' }
#'
#' @details
#' This function uses \code{dplyr::near()} to compare each element of \code{x}
#' to its rounded value, making it robust to floating-point precision issues.
#'
#' @export
#'
#' @examples
#' is_int_like(c(1, 2, 3))
#' is_int_like(c(1.0, 2.0000001, 3.0))
#' is_int_like(c(1.2, 2, 3))
is_int_like <- function(x) {

    # if (!is.numeric(x)) return(FALSE) # does not work with sparse
    #y <- as.vector(dplyr::near(abs(x - round(x)), 0))
    return(all(dplyr::near(abs(x - round(x)), 0), na.rm = T))
}
