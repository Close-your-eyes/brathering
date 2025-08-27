#' Matrix to long data frame
#'
#' @param x
#' @param rownames_to
#' @param colnames_to
#' @param values_to
#'
#' @returns
#' @export
#'
#' @examples
mat_to_df_long <- function(x,
                           rownames_to = "rname",
                           colnames_to = "cname",
                           values_to = "value") {
    x |>
        as.data.frame() |>
        tibble::rownames_to_column(rownames_to) |>
        tidyr::pivot_longer(
          -dplyr::all_of(rownames_to),
          names_to = colnames_to,
          values_to = values_to
        )
}
