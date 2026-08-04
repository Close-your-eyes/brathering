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
#' df_long_to_mat_legacy(df)
df_long_to_mat <- function(
        df,
        to_rows = names(df)[1L],
        to_cols = names(df)[2L],
        values  = names(df)[3L]) {
    stopifnot(
        is.data.frame(df),
        length(to_rows) == 1L,
        length(to_cols) == 1L,
        length(values) == 1L,
        all(c(to_rows, to_cols, values) %in% names(df))
    )

    rows <- df[[to_rows]]
    cols <- df[[to_cols]]
    vals <- df[[values]]

    # Factors follow their level order; other types follow appearance order.
    row_names <- if (is.factor(rows)) {
        levels(rows)[levels(rows) %in% rows]
    } else {
        unique(rows)
    }

    col_names <- if (is.factor(cols)) {
        levels(cols)[levels(cols) %in% cols]
    } else {
        unique(cols)
    }

    row_index <- match(rows, row_names)
    col_index <- match(cols, col_names)

    # A linear index follows R's native column-major matrix layout.
    index <- row_index + (col_index - 1L) * length(row_names)

    # add option to use make.unique to modify row col combinations instaed of rempving them?
    # Retain the first occurrence of each row-column combination.
    keep <- !duplicated(index)

    if (!all(keep)) {
        n_duplicates <- sum(!keep)
        message(
            n_duplicates,
            if (n_duplicates == 1L) {
                " duplicate row-column combination was removed; "
            } else {
                " duplicate row-column combinations were removed; "
            },
            "the first value for each combination was retained."
        )

        index <- index[keep]
        vals  <- vals[keep]
    }
    # if (anyDuplicated(index)) {
    #     stop(
    #         "`df` contains duplicate row-column combinations.",
    #         call. = FALSE
    #     )
    # }

    # Create an appropriately typed missing value.
    mat <- matrix(
        vals[NA_integer_],
        nrow = length(row_names),
        ncol = length(col_names),
        dimnames = list(
            as.character(row_names),
            as.character(col_names)
        )
    )

    mat[index] <- vals
    return(mat)
}

#' @rdname df_long_to_mat
df_long_to_mat_legacy <- function(df,
                                  to_rows = names(df)[1],
                                  to_cols = names(df)[2],
                                  values = names(df)[3]) {
    mat <-
        df |>
        dplyr::select(dplyr::all_of(c(to_rows, to_cols, values))) |>
        tidyr::pivot_wider(names_from = !!rlang::sym(to_cols), values_from = !!rlang::sym(values)) |>
        tibble::column_to_rownames(to_rows) |>
        as.matrix()
    if (is.factor(df[[to_rows]])) {
        lvl <- levels(df[[to_rows]])
        lvl <- lvl[which(lvl %in% rownames(mat))]
        mat <- mat[lvl,]
    }
    if (is.factor(df[[to_cols]])) {
        lvl <- levels(df[[to_cols]])
        lvl <- lvl[which(lvl %in% colnames(mat))]
        mat <- mat[,lvl]
    }
    return(mat)
}
