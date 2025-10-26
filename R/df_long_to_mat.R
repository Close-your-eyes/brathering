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
#' df <- expand.grid(rname = letters[1:5], cname = LETTERS[5:1])
#' df$value <- 1:25
#' df_long_to_mat(df)
df_long_to_mat <- function(df,
                           to_rows = "rname",
                           to_cols = "cname",
                           values = "value") {
    mat <-
        df |>
        dplyr::select(dplyr::all_of(c(to_rows, to_cols, values))) |>
        tidyr::pivot_wider(names_from = !!rlang::sym(to_cols), values_from = !!rlang::sym(values)) |>
        tibble::column_to_rownames(to_rows) |>
        as.matrix()
    if (is.factor(df[[to_rows]])) {
        mat <- mat[levels(df[[to_rows]]),]
    }
    if (is.factor(df[[to_cols]])) {
        mat <- mat[,levels(df[[to_cols]])]
    }
    return(mat)
}
