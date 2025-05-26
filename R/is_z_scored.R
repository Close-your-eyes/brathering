#' Test if a vector is z-scored
#'
#' @param x vector
#' @param tol tolerance for mean and sd
#'
#' @return logical
#' @export
#'
#' @examples
#' is_z_score(rnorm(100))
is_z_scored <- function(x,
                        tol = 1e-2) {
    abs(mean(x, na.rm = TRUE)) < tol && abs(sd(x, na.rm = TRUE) - 1) < tol
}
