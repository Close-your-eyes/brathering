#' Create a membership count matrix from a list of vectors
#'
#' Build a matrix with one row per distinct element and one column per list
#' entry. Values count how often an element occurs in each entry.
#'
#' @param x A list of character vectors. Each list entry defines one column.
#'   List names, when present, are used as column names. To work with individual
#'   letters in strings, split the strings into character vectors first.
#' @param as Output format: \code{"sparse"} (the default) returns a sparse
#'   matrix from the \pkg{Matrix} package; \code{"dense"} returns an ordinary
#'   numeric matrix.
#' @param verbose Logical; whether to print messages about unnamed input, the
#'   number of distinct elements, and sparsity. If \code{TRUE}, an unnamed list
#'   is also assigned sequential numeric column names, stored as character
#'   strings. If \code{FALSE}, unnamed input produces no column names.
#'
#' @details
#' Rows contain the distinct elements observed across all list entries, in
#' order of first appearance. Elements absent from a particular entry have a
#' value of zero. Elements absent from the entire input are omitted; no fixed
#' alphabet is assumed or validated.
#'
#' Repeated elements within an entry are counted, so the result is not
#' necessarily binary. For presence or absence, remove duplicates from each
#' entry with \code{lapply(x, unique)} before calling this function, or compare
#' the returned matrix with zero to obtain a logical presence matrix.
#'
#' The reported sparsity uses the sum of matrix values. It represents the
#' fraction of zero cells only when no entry contains repeated elements.
#'
#' @returns A numeric matrix with distinct elements as row names and list
#'   entries as columns. By default, this is a sparse matrix from \pkg{Matrix};
#'   with \code{as = "dense"}, it is an ordinary matrix. Nonzero values are
#'   occurrence counts.
#'
#' @export
#'
#' @examples
#' x <- list(first = c("A", "C", "A"), second = c("C", "D"))
#'
#' # Counts: A occurs twice in the first column.
#' membership_matrix(x, as = "dense", verbose = FALSE)
#'
#' # Binary membership: remove duplicates within each list entry.
#' membership_matrix(lapply(x, unique), as = "dense", verbose = FALSE)
#'
#' # Amino acid sequences: split each sequence into individual letters.
#' sequences <- c(seq1 = "ACA", seq2 = "CD")
#' membership_matrix(strsplit(sequences, "", fixed = TRUE),
#'                   as = "dense", verbose = FALSE)
membership_matrix <- function(x,
                              as = c("sparse", "dense"),
                              verbose = T) {
    brathering:::.ensure_packages(c("Matrix"))

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
