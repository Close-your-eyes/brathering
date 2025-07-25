#' Floor to a given decimal place
#'
#' @param x numeric vector
#' @param accuracy decimal accuracy
#' @param fun function
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' floor2(rnorm(20))
floor2 <- function(x,
                   accuracy = 0.1,
                   fun = base::floor) {
    fun(x/ accuracy) * accuracy
}

#' Ceiling to a given decimal place
#'
#' @param x numeric vector
#' @param accuracy decimal accuracy
#' @param fun function
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' ceiling2(rnorm(20))
ceiling2 <- function(x,
                     accuracy = 0.1,
                     fun = base::ceiling) {
    fun(x/ accuracy) * accuracy
}
