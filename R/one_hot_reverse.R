#' Reverse one-hot encoding
#'
#' Converts a one-hot encoded data frame or matrix back into its original
#' categorical representation.
#'
#' When `multiple = FALSE`, each row must contain exactly one positive
#' indicator and the corresponding column name is returned. When
#' `multiple = TRUE`, rows may contain multiple positive indicators and the
#' names of all active columns are concatenated into a single string.
#'
#' @param x A one-hot encoded matrix or data frame. Column names are interpreted
#'   as category labels.
#' @param multiple Logical. If `FALSE` (default), each row must contain exactly
#'   one positive indicator. If `TRUE`, multiple positive indicators per row
#'   are allowed.
#' @param sep Character string used to separate category names when
#'   `multiple = TRUE`.
#'
#' @returns
#' A character vector with one element per row:
#' \itemize{
#'   \item If `multiple = FALSE`, the category corresponding to the active
#'   indicator in each row.
#'   \item If `multiple = TRUE`, a concatenated string of all active categories
#'   in each row.
#' }
#'
#' @export
#'
#' @examples
#' # Standard one-hot encoding
#' x <- data.frame(
#'   cardiac = c(1, 0, 0),
#'   medical = c(0, 1, 0),
#'   surgical = c(0, 0, 1)
#' )
#'
#' one_hot_reverse(x)
#'
#' # Multi-label one-hot encoding
#' y <- data.frame(
#'   cardiac = c(1, 0, 1),
#'   medical = c(1, 1, 0),
#'   surgical = c(0, 1, 1)
#' )
#'
#' one_hot_reverse(y, multiple = TRUE)
#' one_hot_reverse(y, multiple = TRUE, sep = "; ")
one_hot_reverse <- function(x,
                            multiple = FALSE,
                            sep = ", ") {

    if (is.null(colnames(x))) {
        stop("x needs colnames.")
    }

    if (!multiple) {
        row_sums <- rowSums(x)

        if (any(row_sums != 1)) {
            stop("Each row must contain exactly one 1.")
        }
        return(colnames(x)[max.col(x)])
    } else {
        return(apply(x, 1, function(row) paste(colnames(x)[as.logical(row)], collapse = sep)))
    }


}
