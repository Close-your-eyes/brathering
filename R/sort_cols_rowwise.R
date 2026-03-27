#' Sort values across columns
#'
#' E.g. to find duplicates which only vary by order.
#'
#' @param df data frame
#' @param cols columns to sort
#'
#' @returns data frame
#' @export
#'
#' @examples
#' df <- data.frame(
#'     queryHits   = c(5, 22, 3),
#'     subjectHits = c(2,  1, 8),
#'     score       = c(0.9, 0.5, 0.7)
#' )
#' sort_cols_rowwise(df) # uses first 2 cols by default
sort_cols_rowwise <- function(df, cols = NULL) {

    if (!is.data.frame(df)) {
        stop("df must be data frame.")
    }

    if (ncol(df) < 2) {
        stop("df needs at least 2 columns.")
    }

    if (is.null(cols)) {
        cols <- names(df)[1:2]
    }

    if (any(!cols %in% names(df))) {
        stop("not all cols found in df.")
    }

    if (length(cols) == 1) {
        return(df)
    } else if (length(cols) == 2) {
        # vectorized
        a <- df[[cols[1]]]
        b <- df[[cols[2]]]
        df[[cols[1]]] <- pmin(a, b)
        df[[cols[2]]] <- pmax(a, b)
    } else {
        # generalized for any no of cols
        df[cols] <- t(apply(df[cols], 1, sort))
    }

    return(df)
}
