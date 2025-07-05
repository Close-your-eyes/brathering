#' Extract regex from string with context
#'
#' Add context to regex matches in a string.
#'
#' @param x character vector of length 1
#' @param pattern regular expression
#' @param before number of char upstream to extract
#' @param after number of char downstream to extract
#'
#' @return character vector
#' @export
#'
#' @examples
#' txt <- "The quick brown fox jumps over the lazy dog."
#' strextract(x = txt, pattern = "o[[:alpha:]]{1}", before = 2, after = 2)
strextract <- function(x, pattern, before = 0, after = 0) {

    match_pos <- gregexpr(pattern, x, perl = TRUE)[[1]]
    match_len <- attr(match_pos, "match.length")

    if (identical(match_pos, -1L)) return(character(0))
    txt_len <- nchar(x)

    # build contexts
    contexts <- mapply(function(st, len) {
        # compute context window
        start_ctx <- max(1, st - before)
        end_ctx   <- min(txt_len, st + len - 1 + after)
        substr(x, start_ctx, end_ctx)
    }, st = match_pos, len = match_len, USE.NAMES = FALSE)

    return(contexts)
}
