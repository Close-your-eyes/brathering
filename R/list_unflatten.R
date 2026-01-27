#' Reverse purrr::list_flatten
#'
#' @param x a list of depth one
#' @param sep separator in names of x used in name_spec of purrr::list_flatten
#'
#' @returns list of lists
#' @export
#'
#' @examples
#' orig <- list(
#' a = list(
#'     x = 1,
#'     y = 2
#' ),
#' b = list(
#'     z = 3
#' )
#' )
#' flat <- purrr::list_flatten(orig)
#' orig2 <- list_unflatten(flat)
#' identical(orig, orig2)
list_unflatten <- function(x, sep = "_") {
    stopifnot(is.list(x), !is.null(names(x)))

    paths <- strsplit(names(x), sep, fixed = TRUE)

    Reduce(
        f = function(acc, i) {
            path <- paths[[i]]
            val <- x[[i]]

            purrr::modify_in(acc, path, ~ val, .default = list())
        },
        x = seq_along(x),
        init = list()
    )
}
