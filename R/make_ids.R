#' Generate Unique Character IDs
#'
#' Generate deterministic, fixed-width IDs using a supplied alphabet.
#' The ID width is the smallest number of characters needed to provide
#' at least `n` distinct IDs.
#'
#' @param n A single finite, non-negative whole number specifying the
#'   number of IDs to generate.
#' @param alphabet A character vector containing at least two distinct,
#'   non-missing, single-character strings. Defaults to lowercase English
#'   letters followed by digits.
#' @param x provide elements to be recoded instead of n. when x is provided
#'   it overrides n. and a named vector is returned then.
#'
#' @details
#' IDs are ordered according to `alphabet`, with the rightmost character
#' changing fastest. The first alphabet character acts as leading padding.
#' All IDs within a result have the same width and are unique.
#'
#' Results are deterministic for a given `n` and `alphabet`. Changing `n`
#' can change the width and therefore the generated IDs.
#'
#' The implementation uses [base::expand.grid()] to generate all
#' combinations at the required width before selecting the first `n`.
#' Memory usage can therefore be substantial, especially when the number
#' of combinations greatly exceeds `n`.
#'
#' @returns A character vector of length `n`. Returns `character(0)`
#'   when `n` is zero.
#'
#' @export
#'
#' @examples
#' make_ids(5)
#' # Returns: "a" "b" "c" "d" "e"
#'
#' make_ids(5, alphabet = c("a", "b"))
#' # Returns: "aaa" "aab" "aba" "abb" "baa"
#'
#' make_ids(x = c("a", "a", "c"), alphabet = c("a", "b"))
#'
#' make_ids(0)
make_ids <- function(
        n,
        x = NULL,
        alphabet = c(letters, as.character(0:9))) {

    # stringr::str_pad() ?

    if (!is.null(x)) {
        x <- unique(x[!is.na(x)])
        n <- length(x)
    }

    stopifnot(
        is.numeric(n), length(n) == 1L, is.finite(n),
        n >= 0, n == floor(n),
        is.character(alphabet), length(alphabet) >= 2L,
        !anyNA(alphabet), !anyDuplicated(alphabet),
        all(nchar(alphabet) == 1L)
    )

    if (n == 0) return(character(0))

    width <- 1L
    capacity <- length(alphabet)

    while (capacity < n) {
        width <- width + 1L
        capacity <- capacity * length(alphabet)
    }

    grid <- expand.grid(
        rep(list(alphabet), width),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )

    # Reverse columns to match your original ID order.
    grid <- grid[seq_len(n), rev(seq_len(width)), drop = FALSE]

    gr <- do.call(paste0, grid)

    if (!is.null(x)) {
        gr <- stats::setNames(gr, x)
    }

    return(gr)
}


