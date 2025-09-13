#' Get all duplicate rows for inspection
#'
#' Uses dplyr::add_count. Better than any solution with duplicated()
#'
#' @param x data frame
#' @param cols column names to check for duplicates
#'
#' @returns data frame
#' @export
#'
#' @examples
#' df <- data.frame(x = c(letters, letters), y = c(LETTERS, LETTERS))
#' filter_duplicate_rows(df)
filter_duplicate_rows <- function(x, cols = names(x)) {

    prevnames <- names(x)

    x <- dplyr::add_count(x, dplyr::across(dplyr::all_of(cols)))

    newname <- setdiff(names(x), prevnames)

    return(dplyr::filter(x, !!rlang::sym(newname) > 1))
}


