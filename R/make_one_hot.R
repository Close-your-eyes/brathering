#' Create one-hot encoded variables
#'
#' Converts a character, factor, or other atomic vector into a one-hot encoded
#' data frame. Each unique value in the input becomes a column, and each row
#' contains a 1 for the observed category and 0 otherwise.
#'
#' If a data frame is supplied, a column containing the categorical variable
#' must be specified via `col`, unless the data frame contains exactly one
#' column.
#'
#' @param x A vector to encode, or a data frame containing the variable to
#'   encode.
#' @param col Optional character string giving the name of the column in `x`
#'   to encode when `x` is a data frame. If `x` has a single column, that
#'   column is used automatically.
#'
#' @returns A data frame containing one-hot encoded indicator variables. Column
#'   names correspond to the unique values found in the input.
#'
#' @export
#'
#' @examples
#' # Encode a vector
#' make_one_hot(c("a", "b", "a", "c"))
#'
#' # Encode a factor
#' make_one_hot(factor(c("low", "high", "low")))
#'
#' # Encode a column from a data frame
#' df <- data.frame(group = c("a", "b", "a", "c"))
#' make_one_hot(df)
#'
#' # Specify a column in a multi-column data frame
#' df <- data.frame(
#'   id = 1:3,
#'   group = c("a", "b", "a")
#' )
#' make_one_hot(df, col = "group")
make_one_hot <- function(x, col = NULL) {

    if (is.data.frame(x)) {
        if (is.null(col) && ncol(x) > 1) {
            stop("col missing.")
        }
        if (ncol(x) == 1) {
            col <- names(x)[1]
        }
        if (!col %in% names(x)) {
            stop("col not found.")
        }
        x <- x[[col]]
    } else if (!is.vector(x)) {
        stop("x must be vector or data frame.")
    }



    lvls <- unique(x)
    onehot <- purrr::map_dfr(x, ~as.data.frame(matrix(as.numeric(.x == lvls), nrow = 1)))

    # mdel.matrix does not preserve row order
    # onehot <- as.data.frame(model.matrix(~ x - 1))

    names(onehot) <- lvls

    return(onehot)
}
