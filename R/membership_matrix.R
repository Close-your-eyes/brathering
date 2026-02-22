#' Get membership matrix from list of vectors
#'
#' @param x list of character vectors
#' @param as how to return
#'
#' @returns (sparse) matrix
#' @export
#'
#' @examples
membership_matrix <- function(x,
                              as = c("sparse", "dense"),
                              verbose = T) {

    if (!is.list(x)) {
        stop("x has to be a list.")
    }
    if (verbose && is.null(names(x))) {
        message("list without names. assigning numeric names.")
        names(x) <- as.character(seq_along(x))
    }

    as <- rlang::arg_match(as)
    universe <- unique(unlist(x))
    if (verbose) {
        message(length(universe), " unique elements.")
    }

    i <- match(unlist(x), universe)
    j <- rep(seq_along(x), lengths(x))

    M <- Matrix::sparseMatrix(
        i = i,
        j = j,
        x = 1,
        dims = c(length(universe), length(x)),
        dimnames = list(universe, names(x))
    )

    if (verbose) {
        message("sparsity (freq of zeros): ", round(1-sum(M)/length(M), 4))
    }

    if (as == "dense") {
        M <- as.matrix(M)
    }
    return(M)

}
