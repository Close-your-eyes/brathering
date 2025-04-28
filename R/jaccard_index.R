#' Simple jaccard index of elements of 2 vectors
#'
#' @param a vector 1
#' @param b vector 2
#'
#' @return a number representing the jaccard index
#' @export
#'
#' @examples
#' a <- letters[1:5]
#' b <- letters[1:7]
#' jaccard_index(a,b)
jaccard_index <- function(a, b) {
    intersection <- length(intersect(a, b))
    union <- length(union(a, b))
    return(intersection / union)
}
