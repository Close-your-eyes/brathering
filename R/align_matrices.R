#' Align Two Matrices by Row and Column Names
#'
#' Makes two matrices compatible for element-wise addition or subtraction.
#' Missing rows and columns are added and filled with zeros, then both matrices
#' are reordered to have identical dimensions.
#'
#' @param a A numeric matrix with unique, non-missing row and column names.
#' @param b A numeric matrix with unique, non-missing row and column names.
#' @param sort_names Logical. If `TRUE`, row and column names are sorted
#'   alphabetically. If `FALSE`, names retain their first-occurrence order
#'   across `a` and `b`.
#'
#' @return A named list containing:
#' \describe{
#'   \item{a}{The expanded and reordered version of `a`.}
#'   \item{b}{The expanded and reordered version of `b`.}
#' }
#'
#' @details
#' The union of the row names and column names from both matrices determines
#' the output dimensions. Entries corresponding to missing rows or columns are
#' filled with zero.
#'
#' Both matrices must have unique, non-`NA` row and column names.
#'
#' @examples
#' m1 <- matrix(
#'   1:4, nrow = 2,
#'   dimnames = list(c("r1", "r2"), c("x", "y"))
#' )
#'
#' m2 <- matrix(
#'   5:8, nrow = 2,
#'   dimnames = list(c("r2", "r3"), c("y", "z"))
#' )
#'
#' aligned <- align_matrices(m1, m2)
#'
#' aligned$a + aligned$b
#' aligned$a - aligned$b
#'
#' @export
align_matrices <- function(a, b, sort_names = FALSE) {
    stopifnot(is.matrix(a), is.matrix(b))

    validate_names <- function(x, label) {
        if (is.null(rownames(x)) || is.null(colnames(x)))
            stop(label, " must have rownames and colnames.")
        if (anyNA(rownames(x)) || anyDuplicated(rownames(x)))
            stop(label, " must have unique, non-NA rownames.")
        if (anyNA(colnames(x)) || anyDuplicated(colnames(x)))
            stop(label, " must have unique, non-NA colnames.")
    }

    validate_names(a, "a")
    validate_names(b, "b")

    rows <- union(rownames(a), rownames(b))
    cols <- union(colnames(a), colnames(b))

    if (sort_names) {
        rows <- sort(rows)
        cols <- sort(cols)
    }

    expand <- function(x) {
        out <- matrix(
            0,
            nrow = length(rows),
            ncol = length(cols),
            dimnames = list(rows, cols)
        )

        out[rownames(x), colnames(x)] <- x
        out
    }

    list(a = expand(a), b = expand(b))
}
