#' Highly efficient pairwise intersect
#'
#' @param x list of character vectors
#' @param as how to return
#'
#' @returns matrix
#' @export
#'
#' @examples
intersect_pairwise <- function(x,
                               as = c("sparse", "dense")) {
    as <- rlang::arg_match(as)

    M <- membership_matrix(x)
    intersections <- Matrix::crossprod(M)
    if (as == "dense") {
        intersections <- as.matrix(intersections)
    }
    return(intersections)
}
