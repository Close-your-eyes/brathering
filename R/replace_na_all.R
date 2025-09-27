#' Replace all NA values in data frame
#'
#' @param df data frame
#' @param value replacement values
#' @param cols which columns to scan
#' @param exclude columns to exclude
#'
#' @returns data frame
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- tibble::tibble(
#'     id       = 1:5,
#'     score    = c(10, NA, 25, NA, 40),
#'     name     = c("Alice", NA, "Charlie", "David", NA),
#'     category = c("A", "B", NA, "A", "C")
#' )
#' replace_na_all(df)
#' replace_na_all(df, value = "0", cols = dplyr::where(is.character))
#' replace_na_all(df, value = "0", cols = dplyr::where(is.character), exclude = "name")
#' # error because of mixed column types
#' # replace_na_all(df, value = "0", cols = dplyr::everything())
replace_na_all <- function(df,
                           value = 0,
                           cols = dplyr::where(is.numeric),
                           exclude = NULL) {
    df %>%
        dplyr::mutate(
            dplyr::across(
                {{ cols }} & !{{ exclude }},
                ~ tidyr::replace_na(., value)
            )
        )
}
