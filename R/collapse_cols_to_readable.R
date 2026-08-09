#' Collapse grouped column values into readable strings
#'
#' Groups a data frame by one column and collapses the values in every
#' remaining column into a single string. Values can optionally be reduced
#' to unique values and sorted before being collapsed.
#'
#' The function checks that the collapse string does not already occur in
#' any data column. This avoids ambiguity if the resulting strings are later
#' split using the same delimiter.
#'
#' @param data A data frame.
#' @param group_col A column used to group the rows. Supply either an
#'   unquoted column name or a single character string.
#' @param unique_values A single logical value. If `TRUE`, duplicate values
#'   within each group and column are removed before collapsing.
#' @param sort_values A single logical value. If `TRUE`, values within each
#'   group and column are sorted before collapsing. Missing values are placed
#'   last.
#' @param collapse A single non-empty, non-missing character string used to
#'   separate collapsed values. The string must not already occur in any
#'   column of `data`.
#'
#' @return A data frame with one row per distinct value of `group_col`.
#'   The grouping column is retained, while every other column is returned
#'   as a collapsed character column.
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'     id = c(1, 1, 2, 2),
#'     letter = c("b", "a", "c", "c"),
#'     value = c(20, 10, 30, 30)
#' )
#'
#' collapse_cols_to_readable(data, "id")
#' expand_readable_cols(collapse_cols_to_readable(data, "id"), "id")
#'
#' collapse_cols_to_readable(
#'     data,
#'     "id",
#'     unique_values = FALSE,
#'     sort_values = FALSE,
#'     collapse = ";"
#' )
collapse_cols_to_readable <- function(
        data,
        group_col,
        unique_values = FALSE,
        sort_values = FALSE,
        reversible = TRUE,
        collapse = ",") {
    if (!is.data.frame(data)) {
        stop("`data` must be a data frame.", call. = FALSE)
    }

    group_name <- rlang::as_name(rlang::ensym(group_col))

    if (!group_name %in% names(data)) {
        stop(
            sprintf("Grouping column `%s` was not found in `data`.", group_name),
            call. = FALSE
        )
    }

    if (!is.logical(unique_values) ||
        length(unique_values) != 1L ||
        is.na(unique_values)) {
        stop("`unique_values` must be TRUE or FALSE.", call. = FALSE)
    }

    if (!is.logical(sort_values) ||
        length(sort_values) != 1L ||
        is.na(sort_values)) {
        stop("`sort_values` must be TRUE or FALSE.", call. = FALSE)
    }

    if (!is.character(collapse) ||
        length(collapse) != 1L ||
        is.na(collapse) ||
        !nzchar(collapse)) {
        stop(
            "`collapse` must be a single, non-empty, non-missing string.",
            call. = FALSE
        )
    }

    if (reversible & (unique_values | sort_values)) {
        stop("for reversibility unique_values and sort_values should be FALSE.", call. = FALSE)
    }

    contains_collapse <- vapply(
        data,
        \(x) any(
            grepl(collapse, as.character(x), fixed = TRUE),
            na.rm = TRUE
        ),
        logical(1)
    )

    if (any(contains_collapse)) {
        offending_columns <- names(data)[contains_collapse]

        stop(
            sprintf(
                "`collapse` (%s) already occurs in column(s): %s.",
                encodeString(collapse, quote = '"'),
                paste(offending_columns, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::everything(),
                \(x) {
                    if (unique_values) {
                        x <- unique(x)
                    }

                    if (sort_values) {
                        x <- sort(x, na.last = TRUE)
                    }

                    paste(x, collapse = collapse)
                }
            ),
            .by = dplyr::all_of(group_name)
        )
}


#' Expand collapsed columns into rows
#'
#' Reverses the row-collapsing operation performed by
#' [collapse_cols_to_readable()] by splitting every non-grouping column on a
#' specified delimiter and expanding the resulting values into rows.
#'
#' Within each input row, split columns must have either one value or the same
#' maximum number of values. Length-one columns are recycled during expansion.
#'
#' This operation is only a true inverse when the original data was collapsed
#' with `unique_values = FALSE` and `sort_values = FALSE`. Column types and the
#' distinction between missing values and the literal string `"NA"` cannot be
#' recovered from the collapsed character representation.
#'
#' @param data A data frame produced by [collapse_cols_to_readable()].
#' @param group_col The grouping column that should not be split. Supply either
#'   an unquoted column name or a single character string.
#' @param collapse A single non-empty, non-missing character string used to
#'   split collapsed values. It must match the delimiter used by
#'   [collapse_cols_to_readable()].
#'
#' @return A data frame in which the collapsed columns have been split and
#'   expanded into rows. The grouping column retains its original type; all
#'   expanded columns are character columns.
#'
#' @export
#'
#' @examples
#' original <- data.frame(
#'     id = c(1, 1, 2, 2),
#'     letter = c("b", "a", "c", "c"),
#'     value = c(20, 10, 30, 30)
#' )
#'
#' collapsed <- collapse_cols_to_readable(
#'     original,
#'     id,
#'     unique_values = FALSE,
#'     sort_values = FALSE,
#'     collapse = " | "
#' )
#'
#' expand_readable_cols(
#'     collapsed,
#'     id,
#'     collapse = " | "
#' )
expand_readable_cols <- function(
        data,
        group_col,
        collapse = ","
) {
    if (!is.data.frame(data)) {
        stop("`data` must be a data frame.", call. = FALSE)
    }

    group_name <- rlang::as_name(rlang::ensym(group_col))

    if (!group_name %in% names(data)) {
        stop(
            sprintf("Grouping column `%s` was not found.", group_name),
            call. = FALSE
        )
    }

    if (!is.character(collapse) ||
        length(collapse) != 1L ||
        is.na(collapse) ||
        !nzchar(collapse)) {
        stop(
            "`collapse` must be a single, non-empty, non-missing string.",
            call. = FALSE
        )
    }

    value_names <- setdiff(names(data), group_name)

    if (!length(value_names)) {
        return(data)
    }

    non_character <- value_names[
        !vapply(data[value_names], is.character, logical(1))
    ]

    if (length(non_character)) {
        stop(
            sprintf(
                "Collapsed columns must be character columns: %s.",
                paste(non_character, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    split_data <- data |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(value_names),
                \(x) strsplit(x, split = collapse, fixed = TRUE)
            )
        )

    incompatible <- apply(
        split_data[value_names],
        1L,
        \(row) {
            lengths <- lengths(row)
            target_length <- max(lengths)

            any(!lengths %in% c(1L, target_length))
        }
    )

    if (any(incompatible)) {
        stop(
            sprintf(
                paste0(
                    "Collapsed columns have incompatible lengths in ",
                    "input row(s): %s."
                ),
                paste(which(incompatible), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    split_data |>
        tidyr::unnest(
            cols = dplyr::all_of(value_names)
        )
}
