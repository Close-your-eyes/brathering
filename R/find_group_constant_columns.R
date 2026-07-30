#' Find columns that are constant within groups
#'
#' Same as get_unique_level_columns.
#'
#' Identifies columns that contain exactly one distinct value within every group
#' defined by `group_col`. The grouping column is always included in the result.
#'
#' Missing values are treated as distinct values. Consequently, a group containing
#' both a missing and a non-missing value is not considered constant.
#'
#' @param df A data frame.
#' @param group_col A single character string naming the column used to group
#'   observations.
#'
#' @return A character vector containing `group_col` and the names of columns
#'   that are constant within every group.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   customer_id = c(1, 1, 2, 2),
#'   country = c("DE", "DE", "FR", "FR"),
#'   purchase = c(10, 20, 15, 30)
#' )
#'
#' find_group_constant_columns(df, "customer_id")
#' # Returns: "customer_id" "country"
find_group_constant_columns <- function(df, group_col) {
    if (!group_col %in% names(df)) {
        stop("group_col not in df")
    }
    col_level_df <- dplyr::summarise(df, dplyr::across(.cols = dplyr::everything(), ~dplyr::n_distinct(.x)), .by = !!rlang::sym(group_col))
    uniquelevelcols <- c(group_col, names(which(apply(col_level_df == 1, 2, all))))
    return(uniquelevelcols)
}
