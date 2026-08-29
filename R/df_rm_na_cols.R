#' Remove Completely Missing Columns
#'
#' Removes columns in which every value is missing.
#'
#' @param df A data frame or tibble.
#'
#' @return A data frame containing only columns with at least one non-missing
#'   value.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = 1:3,
#'   y = c(NA, NA, NA),
#'   z = c("a", NA, "c")
#' )
#'
#' df_rm_na_cols(df)
df_rm_na_cols <- function(df) {
    na_cols <- which(apply(df, 2, function(x) sum(is.na(x)))/nrow(df) == 1)
    df <- df[,-na_cols]
    return(df)
}


#' Remove Constant Columns
#'
#' Removes columns containing only one distinct value. Missing values count as
#' distinct values, so a column containing both a constant value and `NA` is
#' retained. Columns containing only `NA` are considered constant and removed.
#'
#' @param df A data frame or tibble.
#'
#' @return A data frame containing only columns with more than one distinct
#'   value.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = 1:3,
#'   y = rep("constant", 3),
#'   z = c("a", "a", NA)
#' )
#'
#' df_rm_eq_cols(df)
df_rm_eq_cols <- function(df) {
    eq_cols <- which(apply(df, 2, function(x) length(unique(x)) == 1))
    df <- df[,-eq_cols]
    return(df)
}
