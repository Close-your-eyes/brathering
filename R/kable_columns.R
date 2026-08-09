#' Display a Data Frame in Multiple Column Blocks
#'
#' Reshapes a data frame or matrix into side-by-side column blocks before
#' formatting it with [knitr::kable()]. Rows continue in the next block after
#' `rows_per_page` rows.
#'
#' If `n_pages` is provided, it takes precedence over `rows_per_page`. The
#' number of rows per block is then calculated as
#' `ceiling(nrow(x) / n_pages)`.
#'
#' @param x A data frame or matrix to display.
#' @param rows_per_page A positive integer specifying the maximum number of
#'   rows in each column block. Ignored when `n_pages` is not `NULL`.
#' @param n_pages Either `NULL` or a positive integer specifying the number of
#'   side-by-side column blocks. When `NULL`, the number of blocks is determined
#'   automatically from `rows_per_page`.
#' @param ... Additional arguments passed to [knitr::kable()].
#'
#' @return A `knitr_kable` object containing the formatted table.
#'
#' @details
#' The final block is padded with `NA` rows when the number of observations is
#' not evenly divisible by the number of rows per block.
#'
#' @examples
#' # Determine the number of blocks automatically
#' kable_columns(mtcars, rows_per_page = 10)
#'
#' # Create exactly four blocks; rows_per_page is ignored
#' kable_columns(mtcars, n_pages = 4)
#'
#' # Pass formatting options to knitr::kable()
#' kable_columns(mtcars, n_pages = 2, digits = 2)
#'
#' @importFrom knitr kable
#' @export
kable_columns <- function(x, rows_per_page = 10, n_pages = NULL, ...) {
    stopifnot(is.data.frame(x) || is.matrix(x))

    x <- as.data.frame(x)

    if (is.null(n_pages)) {
        stopifnot(rows_per_page > 0)
        n_pages <- ceiling(nrow(x) / rows_per_page)
    } else {
        stopifnot(n_pages > 0)
        rows_per_page <- ceiling(nrow(x) / n_pages)
    }

    pages <- lapply(seq_len(n_pages), function(page) {
        idx <- (page - 1) * rows_per_page + seq_len(rows_per_page)

        page_data <- x[idx, , drop = FALSE]
        names(page_data) <- names(x)
        page_data
    })

    result <- do.call(cbind, pages)
    rownames(result) <- NULL

    knitr::kable(result, ...)
}
