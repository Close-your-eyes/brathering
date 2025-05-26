#' Ceiling to a given decimal place
#'
#' @param x numeric vector
#' @param accuracy decimal accuracy
#' @param f function
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' ceiling2(rnorm(20))
ceiling2 <- function(x,
                        accuracy = 0.1,
                        f = base::ceiling) {
    f(x/ accuracy) * accuracy
}
