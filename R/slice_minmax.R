#' Slice minimum and maximum rows by a variable
#'
#' Returns rows corresponding to the smallest and largest values of
#' `order_by`, combining the results of `dplyr::slice_min()` and
#' `dplyr::slice_max()`. Duplicate rows are removed with
#' `dplyr::distinct()`.
#'
#' @param data A data frame, tibble, or grouped data frame.
#' @param order_by A variable or expression used to rank rows.
#' @param with_ties Logical. Should ties be kept together? Passed to
#'   [dplyr::slice_min()] and [dplyr::slice_max()]. Defaults to `FALSE`.
#' @param n Integer vector specifying the number of rows to return from
#'   the lower and upper tails, respectively. If a single value is
#'   supplied, the same number of rows is returned from both tails.
#'   Defaults to `c(10, 10)`.
#' @param ... Additional arguments passed to
#'   [dplyr::slice_min()] and [dplyr::slice_max()].
#'
#' @returns
#' A data frame containing the rows with the smallest and largest values
#' of `order_by`. Duplicate rows are removed.
#'
#' @details
#' For grouped data frames, slicing is performed within each group,
#' consistent with the behavior of [dplyr::slice_min()] and
#' [dplyr::slice_max()].
#'
#' @export
#'
#' @examples
#' # Symmetric tails
#' slice_minmax(mtcars, mpg, n = 5)
#'
#' # Different numbers from each tail
#' slice_minmax(mtcars, mpg, n = c(3, 10))
#'
#' # Grouped data
#' mtcars |>
#'   dplyr::group_by(cyl) |>
#'   slice_minmax(mpg, n = 2)
slice_minmax <- function(data, order_by, with_ties = FALSE, n = c(10, 10), ...) {
    if (length(n) == 1) {
        n <- c(n, n)
    }

    sub <- dplyr::bind_rows(
        dplyr::slice_min(data, {{ order_by }}, with_ties = with_ties, n = n[1], ...),
        dplyr::slice_max(data, {{ order_by }}, with_ties = with_ties, n = n[2], ...)
    ) |>
        dplyr::distinct()

    sub
}

slice_minmax <- function(data, order_by, with_ties = F, n = c(10,10), ...) {
    if (length(n) == 1) {
        n <- c(n,n)
    }
    sub <- dplyr::bind_rows(
        dplyr::slice_min(data, {{ order_by }}, with_ties = with_ties, n = n[1], ...),
        dplyr::slice_max(data, {{ order_by }}, with_ties = with_ties, n = n[2], ...)) |>
        dplyr::distinct()
    return(sub)
}
