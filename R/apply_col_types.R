#' Record Data Frame Column Types
#'
#' Records the primary class of each column in a data frame. For factor
#' columns, the factor levels are stored in the `"factor_levels"` attribute
#' of the returned vector. The resulting
#' named character vector can be supplied to [apply_coltypes_by_name()] to
#' restore column types after importing or transforming data.
#'
#' @param df A data frame or data-frame-like object.
#'
#' @return A named character vector containing the class of each column.
#'   Factor levels are stored as a named list in the `"factor_levels"`
#'   attribute.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   group = factor(
#'     c("control", "treatment", "control"),
#'     levels = c("control", "treatment", "unknown")
#'   )
#' )
#'
#' coltypes <- record_coltypes(df)
#' coltypes
#' attr(coltypes, "factor_levels")
record_coltypes <- function(df) {
    coltypes <- purrr::map_chr(df, function(x) if(is.factor(x)) "factor" else class(x)[[1]])

    factor_levels <- purrr::map(
        df[vapply(df, is.factor, logical(1))],
        levels
    )

    attr(coltypes, "factor_levels") <- factor_levels

    coltypes
}


#' Apply Column Types by Name
#'
#' Converts selected columns of a data frame to the types specified in a
#' named character vector. When `coltypes` was created by
#' [record_coltypes()], stored factor levels are restored to their respective
#' columns, including levels not present in the current data.
#'
#' Supported types are `"integer"`, `"numeric"`, `"character"`, `"logical"`,
#' `"factor"`, `"Date"`, and `"POSIXct"`. Unrecognized types leave the
#' corresponding columns unchanged.
#'
#' @param df A data frame or data-frame-like object.
#' @param coltypes A named character vector containing the desired column
#'   types. Factor levels may be supplied through its `"factor_levels"`
#'   attribute.
#'
#' @return `df` with the specified columns converted to their requested
#'   types.
#' @export
#'
#' @examples
#' original <- data.frame(
#'   id = 1:3,
#'   group = factor(
#'     c("control", "treatment", "control"),
#'     levels = c("control", "treatment", "unknown")
#'   )
#' )
#'
#' coltypes <- record_coltypes(original)
#'
#' imported <- data.frame(
#'   id = c("1", "2"),
#'   group = c("treatment", "control")
#' )
#'
#' restored <- apply_coltypes_by_name(imported, coltypes)
#' levels(restored$group)
apply_coltypes_by_name <- function(df, coltypes) {
    factor_levels <- attr(coltypes, "factor_levels")

    df[names(coltypes)] <- Map(
        function(name, type) {
            x <- df[[name]]

            switch(
                type,
                integer   = as.integer(x),
                numeric   = as.numeric(x),
                character = as.character(x),
                logical   = as.logical(x),
                factor    = {
                    stored_levels <- factor_levels[[name]]

                    if (is.null(stored_levels)) {
                        as.factor(x)
                    } else {
                        factor(x, levels = stored_levels)
                    }
                },
                Date    = as.Date(x),
                POSIXct = as.POSIXct(x),
                x
            )
        },
        names(coltypes),
        unname(coltypes)
    )

    df
}
