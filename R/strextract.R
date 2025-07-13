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

    # match_pos <- gregexpr(pattern, x, perl = TRUE)[[1]]
    # match_len <- attr(match_pos, "match.length")
    # if (identical(match_pos, -1L)) return(character(0))
    match_pos <- stringr::str_locate_all(x, pattern)[[1]]
    if (nrow(match_pos) == 0) return(character(0))
    match_len <- match_pos[,2]-match_pos[,1]+1


    contexts <- stringr::str_sub(x, pmax(1, match_pos-before), pmin(nchar(x), match_pos+match_len+after))
    # build contexts
    # contexts <- mapply(function(st, len) {
    #     # compute context window
    #     start_ctx <- max(1, st - before)
    #     end_ctx   <- min(txt_len, st + len - 1 + after)
    #     substr(x, start_ctx, end_ctx)
    # }, st = match_pos, len = match_len, USE.NAMES = FALSE)

    return(contexts)
}
