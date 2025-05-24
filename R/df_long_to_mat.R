#' Convert long data frame to matrix format
#'
#' Just for convenience.
#'
#' @param df data frame in long format
#' @param to_rows column to become rows in matrix
#' @param to_cols column to become columns in matrix
#' @param values column to populate the matrix
#'
#' @return a matrix
#' @export
#'
#' @examples
df_long_to_mat <- function(df,
                           to_rows = "rows",
                           to_cols = "cols",
                           values = "values") {
    mat <-
        df |>
        dplyr::select(dplyr::all_of(c(to_rows, to_cols, values))) |>
        tidyr::pivot_wider(names_from = !!rlang::sym(to_cols), values_from = !!rlang::sym(values)) |>
        tibble::column_to_rownames(to_rows) |>
        as.matrix()
    return(mat)
}
