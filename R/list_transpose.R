#' Transpose a List of Equal-Length Vectors
#'
#' Transposes a list of equal-length vectors, effectively swapping rows and columns.
#' This is analogous to matrix transposition, but applied to a list structure.
#'
#' The input must be a list where each element is an atomic vector of the same length.
#' The function returns a new list where the \emph{i}-th element contains the \emph{i}-th
#' element from each input vector. In other words, it converts a list of row vectors
#' into a list of column vectors (or vice versa).
#'
#' @param x list of equal-length vectors
#'
#' @returns
#' A list of the same length as the individual input vectors. Each element of the
#' returned list is a vector containing the corresponding elements across all
#' input vectors.
#' @export
#'
#' @examples
#' x <- list(
#'   c(1, 2, 3),
#'   c(4, 5, 6),
#'   c(7, 8, 9)
#' )
#' list_transpose(x)
list_transpose <- function(x) {
    if (length(unique(lengths(x))) != 1) {
        stop("list elements of x must be of equal length.")
    }
    m <- do.call(rbind, x)
    res <- lapply(seq_len(ncol(m)), function(i) m[, i])
    return(unname(res))

    # same:
    # z <- matrix(unlist(x), ncol = length(x[[1]]), byrow = T)
    # zz <- brathering::split_mat(z, 1:ncol(z), byrow = F)
    # zzz <- unname(purrr::map(zz, ~.x[,1]))
    # return(zzz)
}
