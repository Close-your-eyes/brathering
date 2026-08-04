#' Matrix to long data frame
#'
#' First step towards plotting with ggplot.
#'
#' @param x matrix or wide data frame
#' @param rownames_to column name that will hold rownames(mat)
#' @param colnames_to column name that will hold colnames(mat)
#' @param row_col_type how to return row and col indices
#' @param values_to column name that will hold values of mat
#' @param keep_diagonal set to FALSE to remove entries
#' on the diagonal nrow(x) == ncol(x)
#'
#' @returns data frame in long format
#' @export
#' @importFrom rlang %||%
#'
#' @examples
#' mat <- structure(c(0.29, 0.27, 0.38, 0.37, 0.41, 0.37, 0.4, 0.37, 0.62,
#'                    0.37, 0.39, 0.28, 0.23, 0.22, 0.29, 0.51, 0.22, 0.27, 0.6, 0.26,
#'                    0.24, 0.29, 0.27, 0.26, 0.33, 0.32, 0.35, 0.33, 0.33, 0.32, 0.48,
#'                    0.32, 0.35, 0.25, 0.22, 0.22, 0.26, 0.61, 0.22, 0.24, 0.47, 0.24,
#'                    0.21, 0.27, 0.32, 0.3, 0.39, 0.38, 0.51, 0.38, 0.57, 0.39, 0.33,
#'                    0.38, 0.41, 0.34, 0.25, 0.23, 0.35, 0.33, 0.24, 0.3, 0.34, 0.29,
#'                    0.26, 0.27, 0.37, 0.34, 0.52, 0.47, 0.61, 0.48, 0.57, 0.48, 0.4,
#'                    0.5, 0.54, 0.36, 0.27, 0.25, 0.38, 0.38, 0.26, 0.32, 0.42, 0.3,
#'                    0.28, 0.28, 0.38, 0.35, 0.64, 0.53, 0.55, 0.56, 0.48, 0.53, 0.4,
#'                    0.63, 0.64, 0.36, 0.27, 0.29, 0.37, 0.38, 0.26, 0.33, 0.41, 0.3,
#'                    0.28, 0.28, 0.13, 0.14, 0.12, 0.13, 0.13, 0.13, 0.14, 0.13, 0.12,
#'                    0.12, 0.12, 0.22, 0.51, 0.21, 0.19, 0.13, 0.5, 0.33, 0.11, 0.31,
#'                    0.47, 0.3, 0.19, 0.19, 0.18, 0.2, 0.19, 0.19, 0.19, 0.2, 0.16,
#'                    0.17, 0.18, 0.31, 0.35, 0.27, 0.25, 0.16, 0.34, 0.43, 0.17, 0.44,
#'                    0.34, 0.28, 0.14, 0.15, 0.13, 0.14, 0.14, 0.15, 0.14, 0.15, 0.16,
#'                    0.13, 0.14, 0.17, 0.26, 0.17, 0.17, 0.17, 0.26, 0.22, 0.15, 0.21,
#'                    0.26, 0.46, 0.15, 0.15, 0.16, 0.15, 0.15, 0.15, 0.15, 0.15, 0.13,
#'                    0.16, 0.16, 0.32, 0.21, 0.6, 0.22, 0.15, 0.2, 0.25, 0.12, 0.3,
#'                    0.23, 0.18, 0.59, 0.62, 0.34, 0.35, 0.35, 0.36, 0.35, 0.37, 0.29,
#'                    0.33, 0.33, 0.43, 0.29, 0.28, 0.58, 0.3, 0.28, 0.32, 0.3, 0.32,
#'                    0.28, 0.28, 0.38, 0.36, 0.52, 0.6, 0.47, 0.59, 0.44, 0.58, 0.37,
#'                    0.51, 0.51, 0.39, 0.27, 0.27, 0.4, 0.36, 0.26, 0.33, 0.37, 0.31,
#'                    0.28, 0.28, 0.32, 0.32, 0.3, 0.34, 0.33, 0.33, 0.34, 0.34, 0.25,
#'                    0.29, 0.3, 0.46, 0.28, 0.29, 0.42, 0.26, 0.26, 0.32, 0.25, 0.32,
#'                    0.28, 0.26), dim = c(22L, 12L), dimnames = list(c("0", "1", "2",
#'                                                                      "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
#'                                                                      "15", "16", "17", "18", "19", "20", "21"), c("CD-IC-A", "CD-IC-B",
#'                                                                                                                   "CD-PC", "CNT", "DCT", "EC", "IntC", "Leuk", "Podo", "PT", "TAL",
#'                                                                                                                   "tL")))
#' df <- mat_to_df_long(mat, values_to = "score")
#' plot <- fcexpr::heatmap_long_df(df,
#'                                 groups = names(df)[2],
#'                                 features = names(df)[1],
#'                                 values = names(df)[3])
#' df2 <- mat_to_df_long_legacy(mat, values_to = "score")
#' waldo::compare(df |> dplyr::arrange(rname, cname), df2 |> dplyr::arrange(rname, cname))
mat_to_df_long <- function(
        x,
        rownames_to = "rname",
        colnames_to = "cname",
        values_to   = "value",
        row_col_type = c("factor", "character", "numeric", "char", "num", "f", "c", "n"),
        keep_diagonal = T) {

    row_col_type <- rlang::arg_match(row_col_type)

    nr <- nrow(x)
    nc <- ncol(x)

    rn <- rownames(x) %||% as.character(seq_len(nr))
    cn <- colnames(x) %||% as.character(seq_len(nc))

    # Same row-major ordering as pivot_longer()
    # Follow R's native column-major matrix storage order
    r <- rep(rn, times = nc)
    c <- rep(cn, each = nr)

    if (row_col_type %in% c("f", "factor")) {
        r <- factor(r, levels = unique(rn))
        c <- factor(c, levels = unique(cn))
    } else if (row_col_type %in% c("numeric", "num", "n")) {
        r <- as.numeric(r)
        c <- as.numeric(c)
    } else {
        r <- as.character(r)
        c <- as.character(c)
    }

    names(r) <- NULL
    names(c) <- NULL

    if (!keep_diagonal) {
        keep <- as.vector(row(x) != col(x))

        r <- r[keep]
        c <- c[keep]
        x <- as.vector(x)[keep]
    } else {
        x <- as.vector(x)
    }

    df <- tibble::new_tibble(
        stats::setNames(
            list(r, c, x),
            c(rownames_to, colnames_to, values_to)
        ),
        nrow = length(x)
    )

    return(df)
}

#' @rdname mat_to_df_long
mat_to_df_long_legacy <- function(x,
                                  rownames_to = "rname",
                                  colnames_to = "cname",
                                  values_to = "value",
                                  row_col_type = c("factor", "char", "num", "character", "numeric")) {

    message("legacy version with tidyr::pivot_longer: uses much more memory and is slower.")

    row_col_type <- rlang::arg_match(row_col_type)

    if (is.null(rownames(x))) {
        rownames(x) <- as.character(1:nrow(x))
    }
    if (is.null(colnames(x))) {
        colnames(x) <- as.character(1:ncol(x))
    }
    z <- x |>
        as.data.frame() |>
        tibble::rownames_to_column(rownames_to) |>
        tidyr::pivot_longer(
            -dplyr::all_of(rownames_to),
            names_to = colnames_to,
            values_to = values_to)
    if (row_col_type == "factor") {
        z[[rownames_to]] <- factor(z[[rownames_to]], levels = unique(rownames(x)))
        z[[colnames_to]] <- factor(z[[colnames_to]], levels = unique(colnames(x)))
    } else if (row_col_type %in% c("numeric", "num")) {
        z[[rownames_to]] <- as.numeric(z[[rownames_to]])
        z[[colnames_to]] <- as.numeric(z[[colnames_to]])
    } else if (row_col_type %in% c("character", "char")) {
        z[[rownames_to]] <- as.character(z[[rownames_to]])
        z[[colnames_to]] <- as.character(z[[colnames_to]])
    }

    return(z)
    # dplyr::mutate(!!rlang::sym(rownames_to) := factor(!!rlang::sym(rownames_to), rownames(x))) |>
    # dplyr::mutate(!!rlang::sym(colnames_to) := factor(!!rlang::sym(colnames_to), colnames(x)))
}
