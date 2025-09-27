#' Find columns with unique levels within groups of reference column
#'
#' It finds columns which can be joined via refcol to another df without
#' increasing
#'
#' @param df data frame
#' @param refcol a column in df, intended for joining to another df
#'
#' @returns column names with refcol at first position
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'     ~id, ~category, ~color,   ~size, ~constantA, ~constantB,
#'     1,  "A",       "red",    "S",   "foo",      "same",
#'     2,  "A",       "red",    "M",   "foo",      "same",
#'     3,  "B",       "blue",   "S",   "bar",      "same",
#'     4,  "B",       "blue",   "L",   "bar",      "same"
#' )
#' joincols <- get_unique_level_columns(df, refcol = "category")
#' df2 <- tibble::tribble(
#'     ~category, ~description,         ~priority,
#'     "A",       "Category A items",   1,
#'     "B",       "Category B items",   2,
#'     "C",       "Category C items",   3
#' )
#' df3 <- dplyr::left_join(df2,
#'                         dplyr::distinct(df, dplyr::pick(dplyr::all_of(joincols))),
#'                         by = "category")
#' # without selection for joincols, df4 gets longer
#' df4 <- dplyr::left_join(df2,
#'                         dplyr::distinct(df),
#'                         by = "category")
get_unique_level_columns <- function(df, refcol) {
    if (!refcol %in% names(df)) {
        stop("refcol not in df")
    }
    col_level_df <-
        df |>
        dplyr::group_by(!!rlang::sym(refcol)) |>
        dplyr::summarise(dplyr::across(.cols = dplyr::everything(), ~dplyr::n_distinct(.x)), .groups = "drop")
    uniquelevelcols <- c(refcol, names(which(apply(col_level_df == 1, 2, all))))
    return(uniquelevelcols)
}


# get_unique_level_columns2 <- function(df, refcol, na_rm = FALSE) {
#     stopifnot(refcol %in% names(df))
#     name_sym <- rlang::sym(refcol)
#
#     # Count distincts per group for all non-ref columns
#     counts <-
#         df %>%
#         dplyr::group_by(!!name_sym) %>%
#         dplyr::summarise(
#             dplyr::across(
#                 .cols = -dplyr::all_of(refcol),
#                 .fns  = ~ dplyr::n_distinct(.x, na.rm = na_rm)
#             ),
#             .groups = "drop"
#         )
#
#     # Which columns have count==1 for every group?
#     constant_cols <-
#         counts %>%
#         dplyr::summarise(dplyr::across(dplyr::everything(), ~ all(.x == 1))) %>%
#         tidyr::pivot_longer(dplyr::everything(), names_to = "col", values_to = "is_constant") %>%
#         dplyr::filter(is_constant) %>%
#         dplyr::pull(col)
#
#     # Always include the reference column, avoid duplicates
#     unique(c(refcol, constant_cols))
# }


