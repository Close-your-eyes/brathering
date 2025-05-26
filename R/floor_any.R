#' Floor to a given decimal place
#'
#' @param x numeric vector
#' @param accuracy decimal accuracy
#' @param f function
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' floor2(rnorm(20))
floor2 <- function(x,
                      accuracy = 0.1,
                      f = base::floor) {
    f(x/ accuracy) * accuracy
}
