#' Reorder axis text across facets or any column
#'
#' @param x axis variable
#' @param by ordering variable, probably numeric
#' @param within group variable, e.g. facet variable
#' @param fun what to apply 'by'
#' @param sep internal separator
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = reorder_within(x, num_order_col, group))) +
#'     scale_x_reordered()
#' }
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
}

#' Fix axis text from reorder_within
#'
#' @param ... args to ggplot2::scale_x_discrete
#' @param sep internal separator
#'
#' @returns
#' @export
#'
#' @examples
#' ggplot(df, aes(x = reorder_within(x, num_order_col, group))) +
#'     scale_x_reordered()
#' }
scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#' Fix axis text from reorder_within
#'
#' @param ... args to ggplot2::scale_y_discrete
#' @param sep internal separator
#'
#' @returns
#' @export
#'
#' @examples
#' ggplot(df, aes(y = reorder_within(y, num_order_col, group))) +
#'     scale_y_reordered()
#' }
scale_y_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}
