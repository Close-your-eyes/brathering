#' Evaluate (groupwise) ECDF at a given cutoff (reverse quantile)
#'
#' Related to quantile_filter.
#'
#' @param df data frame.
#' @param groupcol optional grouping column
#' @param valuecol numeric column to build the ECDF(s) on
#' @param cutoff numeric cutoff at which to evaluate ECDF
#' @param strict <cutoff or <=cutoff
#' @param na_rm remove NA?
#'
#' @return A tibble with one row per group and cutoff
#' @export
#' @examples
ecdf_eval <- function(df,
                      valuecol,
                      cutoff,
                      groupcol = NULL,
                      strict = FALSE,
                      na_rm = TRUE) {

    # ECDF cutoff t is the proportion of values ≤ t, i.e. F(t) = mean(x <= t).
    # Doing it this way avoids creating per-group function closures and is very efficient.
    # F(t) = mean(X <= t); optionally strict < t

    if (missing(valuecol) || missing(cutoff)) {
        stop("valuecol and cutoff needed.")
    }
    op <- if (strict) `<` else `<=`

    purrr::map_dfr(stats::setNames(cutoff, cutoff), function(cut) {
        dplyr::summarise(
            df,
            prob = mean(op(!!rlang::sym(valuecol), cut), na.rm = na_rm),
            n = dplyr::n(),
            .by = !!rlang::sym(groupcol))
    }, .id = "cutoff") |>
        dplyr::mutate(cutoff = as.numeric(cutoff))

    ## purrr style and explicit creation of ecdf
    # dfsplit <- split(df, df[[groupcol]])
    # ecdfs <- purrr::map(dfsplit, ~ecdf(.x[[valuecol]]))
    # purrr::map(ecdfs, ~.x(cutoff))
}

