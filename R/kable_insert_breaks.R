#' Expand multiline cells into separate rows
#'
#' @description
#' Splits each cell at newline characters and expands each original row to
#' accommodate its longest cell. Shorter cells are padded with empty strings.
#' Optionally inserts blank rows between original rows.
#'
#' @param df A data frame. All columns are converted to character.
#' @param n A single non-negative whole number specifying how many blank rows
#'   to insert between original rows. Defaults to zero. No blank rows are
#'   added after the last original row.
#'
#' @details
#' Handles LF, CRLF, and CR line endings and preserves trailing empty lines
#' within cells. Missing values remain missing on the first line of their
#' cell; additional lines are padded with empty strings.
#'
#' Column names are preserved and row names are reset. The input data frame
#' is not modified.
#'
#' @return A data frame with character columns and expanded rows, suitable
#'   for display with [knitr::kable()].
#'
#' @examples
#' df <- data.frame(
#'   id = 1:2,
#'   text = c("one\ntwo\nthree", "four"),
#'   note = c("A\nB", "C")
#' )
#'
#' expand_cell_lines(df)
#' expand_cell_lines(df, n = 1)
#'
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   knitr::kable(expand_cell_lines(df, n = 1), row.names = FALSE)
#' }
#'
#' @export
kable_insert_breaks <- function(df, n = 0L) {
    stopifnot(
        is.data.frame(df),
        is.numeric(n), length(n) == 1L,
        is.finite(n), n >= 0, n == floor(n)
    )

    df[] <- lapply(df, as.character)
    if (nrow(df) == 0L || ncol(df) == 0L) return(df)

    rows <- lapply(seq_len(nrow(df)), function(i) {
        cells <- lapply(df, function(column) {
            value <- column[i]
            if (is.na(value)) return(NA_character_)

            value <- gsub("\r\n?", "\n", value)
            # Preserve existing trailing empty lines.
            strsplit(paste0(value, "\n"), "\n", fixed = TRUE)[[1L]]
        })

        height <- max(lengths(cells))
        gap <- if (i < nrow(df)) n else 0L

        cells <- lapply(cells, function(lines) {
            c(lines, rep("", height - length(lines) + gap))
        })

        as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}
