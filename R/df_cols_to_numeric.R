#' Convert Data Frame Columns to Numeric When Possible
#'
#' Converts each column of a data frame to numeric if conversion would not
#' introduce additional missing values. Columns containing non-numeric values
#' are returned unchanged. Existing missing values are preserved.
#'
#' @param df A data frame or tibble.
#'
#' @return A data frame of the same type as `df`. Fully convertible columns are
#'   numeric; all other columns retain their original type.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   numbers = c("1", "2", "3"),
#'   mixed = c("1", "two", "3"),
#'   stringsAsFactors = FALSE
#' )
#'
#' df_cols_to_numeric(df)
df_cols_to_numeric <- function(df) {
    dplyr::mutate(df, dplyr::across(dplyr::everything(), \(x) {
        y <- suppressWarnings(as.numeric(as.character(x)))

        if (any(is.na(y) & !is.na(x))) x else y
    }))

}
