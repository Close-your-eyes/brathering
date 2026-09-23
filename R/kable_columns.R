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
#' @param page_spacing A non-negative integer specifying the number of blank
#'   spaces between column blocks. Defaults to `0` (no extra spacing).
#' @param pad A single value used to fill unused cells in the final block.
#'   Defaults to `""`. Use `NA` to display missing-value padding.
#'   Existing missing values in `x` are unaffected.
#'
#' @return A `knitr_kable` object containing the formatted table.
#'
#' @details
#' The final block is padded with `pad` when the number of observations is
#' not evenly divisible by the number of rows per block. Positive
#' `page_spacing` inserts an unnamed spacer column between blocks, filled with
#' non-breaking spaces so that whitespace is preserved in rendered tables.
#' The rendered gap also includes the table format's normal cell padding.
#'
#' @examples
#' # Determine the number of blocks automatically
#' kable_columns(mtcars, rows_per_page = 10)
#'
#' # Create exactly four blocks; rows_per_page is ignored
#' kable_columns(mtcars, n_pages = 4)
#'
#' # Add five blank spaces between blocks
#' kable_columns(mtcars, n_pages = 2, page_spacing = 5)
#'
#' # Use a dash for unused cells in the final block
#' kable_columns(mtcars, rows_per_page = 10, pad = "-")
#'
#' # Pass formatting options to knitr::kable()
#' kable_columns(mtcars, n_pages = 2, digits = 2)
#'
#' @importFrom knitr kable
#' @export
kable_columns <- function(x, rows_per_page = 10, n_pages = NULL, ..., page_spacing = 0, pad = "") {
    brathering:::.ensure_packages(c("knitr"))
    stopifnot(is.data.frame(x) || is.matrix(x))

    if (!is.numeric(page_spacing) || length(page_spacing) != 1L ||
        is.na(page_spacing) || !is.finite(page_spacing) ||
        page_spacing < 0 || page_spacing != floor(page_spacing) ||
        page_spacing > .Machine$integer.max) {
        stop("`page_spacing` must be a single non-negative integer.", call. = FALSE)
    }

    if (!is.atomic(pad) || length(pad) != 1L || !is.null(dim(pad))) {
        stop("`pad` must be a single value.", call. = FALSE)
    }

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

    if (page_spacing > 0 && length(pages) > 1L) {
        spacer <- strrep("\u00a0", page_spacing)
        pages <- lapply(seq_along(pages), function(page) {
            page_data <- pages[[page]]
            if (page < length(pages)) {
                page_data[[ncol(page_data) + 1L]] <- rep(spacer, nrow(page_data))
                names(page_data)[ncol(page_data)] <- ""
            }
            page_data
        })
    }

    result <- do.call(cbind, pages)
    if (page_spacing > 0 && length(pages) > 1L) {
        names(result) <- unlist(lapply(pages, names), use.names = FALSE)
    }
    rownames(result) <- NULL

    args <- list(...)
    if (!is.na(pad) && nrow(x) < n_pages * rows_per_page) {
        # Keep numeric formatting and alignment when padding makes columns text.
        numeric_columns <- vapply(result, is.numeric, logical(1))
        if (!"align" %in% names(args)) {
            args$align <- ifelse(numeric_columns, "r", "l")
        }
        digits <- if (is.null(args$digits)) getOption("digits") else args$digits
        digits <- rep(digits, length.out = ncol(result))
        for (page in seq_len(n_pages)) {
            unused <- (page - 1) * rows_per_page + seq_len(rows_per_page) > nrow(x)
            if (!any(unused)) next
            cols <- (page - 1) * (ncol(x) + as.integer(page_spacing > 0)) +
                seq_len(ncol(x))
            for (col in cols) {
                values <- result[[col]]
                if (numeric_columns[col]) {
                    format_args <- args$format.args
                    if (is.null(format_args)) format_args <- list()
                    format_args$x <- round(values, digits[col])
                    format_args$trim <- TRUE
                    values <- do.call(format, format_args)
                } else {
                    values <- as.character(values)
                }
                values[is.na(result[[col]])] <- NA_character_
                values[unused] <- as.character(pad)
                result[[col]] <- values
            }
        }
    }

    do.call(knitr::kable, c(list(x = result), args))
}
