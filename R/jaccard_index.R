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
#' abdiv::jaccard(x = c(rep("a", 10), rep("b", 10), rep("c", 10)),
#'                y = c(rep("a", 10), rep("b", 10), rep("c", 10)))
#' # abdiv does not work with letters
#' brathering::jaccard_index(a = c(rep("a", 10), rep("b", 10), rep("c", 10)),
#'                           b = c(rep("a", 10), rep("b", 10), rep("c", 10)))
#' # order irrelevant
#' brathering::jaccard_index(a = c(rep("b", 10), rep("a", 10), rep("c", 10)),
#'                           b = c(rep("a", 10), rep("b", 10), rep("c", 10)))
#' # replicates irrelevant
#' brathering::jaccard_index(a = c("a", "b", "c"),
#'                           b = c("a", "b", "c"))
#' # different lengths no problem
#' brathering::jaccard_index(a = c("a", "b", "c"),
#'                           b = c("a", "b", "c", "d"))
#' # here jaccard decreases in a linear fashion
#' jacc <- purrr::map_dbl(purrr::map(1:24, ~seq(1,.x)), function(x) {
#'     brathering::jaccard_index(a = letters[1:25],
#'                               b = letters[1:25][-x])
#' })
#' plot(jacc)
#' # with a non-intersect substitution in one set jaccard decreases non-linear
#' jacc <- purrr::map_dbl(purrr::map(1:24, ~seq(1,.x)), function(x) {
#'     brathering::jaccard_index(a = letters[1:25],
#'                               b = c(letters[1:25][-x], LETTERS[x]))
#' })
#' plot(jacc)
jaccard_index <- function(a, b) {
    intersection <- length(intersect(a, b))
    union <- length(union(a, b))
    return(intersection / union)
}
