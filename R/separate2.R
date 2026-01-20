#' Tidyr separate but distinct
#'
#' Separate unique values of the column to be separated. Intended to increase
#' speed for large data frames with many repeated values in col.
#'
#' @param data data frame
#' @param col column to separate
#' @param into see tidyr::separate
#' @param sep see tidyr::separate
#' @param ... args to see tidyr::separate
#' @param remove see tidyr::separate
#'
#' @returns data frame
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' n <- 1e6
#' df <- tibble::tibble(
#'     id   = seq_len(n),
#'     code = sample(
#'         sprintf("X%03d-%03d", 1:1000, 1:1000),
#'         size = n,
#'         replace = TRUE
#'     )
#' )
#'
#' separate_plain <- function(df) {
#'     df %>% tidyr::separate(code, into = c("a", "b"), sep = "-")
#' }
#'
#' microbenchmark::microbenchmark(
#'     plain = separate_plain(df),
#'     distinct = separate2(df, code, c("a", "b"), "-"),
#'     times = 5
#' )
separate2 <- function(data, col, into, sep, ..., remove = FALSE) {
    col <- rlang::enquo(col)

    lookup <- data %>%
        dplyr::distinct(!!col) %>%
        tidyr::separate(
            col = !!col,
            into = into,
            sep = sep,
            remove = FALSE,
            ...
        )

    data <- data %>%
        dplyr::left_join(lookup, by = rlang::quo_name(col)) %>%
        { if (remove) dplyr::select(., -!!col) else . }

    return(data)
}






