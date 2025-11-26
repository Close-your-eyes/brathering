#' Vector list to membership matrix or incidence matrix
#'
#' Create a membership matrix where 1s indicate membership.
#' Names of x become rows, unique elements in x become columns.
#'
#' @param x (named) list of vectors
#'
#' @returns sparse binary matrix
#' @export
#'
#' @examples
#' # molecular signature genes
#' msigdb <- scexpr::gsea_get_msigdb()
#' mat <- list_to_binary_matrix(msigdb$sets)
#'
#' # prime factors
#' y <- RcppAlgos::primeFactorizeSieve(1e2)
#' mat2 <- list_to_binary_matrix(y) # not only 1s
list_to_binary_matrix <- function(x) {

    if (is.null(names(x))) {
        names(x) <- as.character(seq_along(x))
    }

    y <- sort(unique(unlist(x)))
    mat <- Matrix::sparseMatrix(
        i = rep(seq_along(x),lengths(x)), # rows
        j = match(unlist(x), y), # columns
        x = 1,
        dims = c(length(x), length(y)),
        dimnames = list(names(x), y)
    )
    return(mat)
}
