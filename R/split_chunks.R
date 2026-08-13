#' Split an Object into Chunks
#'
#' Splits a vector or list into either a specified number of approximately
#' equal chunks or chunks containing a specified number of elements.
#'
#' @param x A vector or list to split.
#' @param chunks A positive whole number giving the approximate number of
#'   chunks. Cannot be used together with `size`.
#' @param size A positive whole number giving the maximum number of elements
#'   per chunk. Cannot be used together with `chunks`.
#'
#' @return A named list containing the chunks. An empty `x` returns an empty
#'   list.
#' @export
#'
#' @examples
#' split_chunks(1:10, chunks = 3)
#' split_chunks(letters[1:10], size = 4)
#' split_chunks(integer(), size = 2)
split_chunks <- function(x, chunks = NULL, size = NULL) {
    if (is.null(chunks) == is.null(size)) {
        stop("Supply exactly one of `chunks` or `size`.", call. = FALSE)
    }

    is_positive_whole_number <- function(value) {
        is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            is.finite(value) &&
            value > 0 &&
            value %% 1 == 0
    }

    if (!is.null(chunks)) {
        if (!is_positive_whole_number(chunks)) {
            stop("`chunks` must be a positive whole number.", call. = FALSE)
        }

        if (length(x) > 0L && chunks > length(x)) {
            stop("`chunks` cannot exceed the length of `x`.", call. = FALSE)
        }
    }

    if (!is.null(size) && !is_positive_whole_number(size)) {
        stop("`size` must be a positive whole number.", call. = FALSE)
    }

    if (length(x) == 0L) {
        return(list())
    }

    if (is.null(size)) {
        size <- length(x) / chunks
    }

    split(x, ceiling(seq_along(x) / size))
}
