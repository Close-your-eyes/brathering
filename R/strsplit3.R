#' Split string at pattern maintaining max length of substrings
#'
#' Similar to stringr::str_wrap.
#'
#' @param x character vector
#' @param width max width of split elements
#' @param sep split symbol to separate at
#'
#' @return list of character vectors
#' @export
#'
#' @examples
#' strsplit3(c("HALLMARK_INTERFERON_ALPHA_RESPONSE", "HALLMARK_INTERFERON_GAMMA_RESPONSE"), 20)
strsplit3 <- function(x, width = 30, sep = "_") {
    split_string <- strsplit(x, sep)

    lines <- purrr::map(split_string, function(y) {
        lines <- character(0)
        current_line <- ""
        for (word in unlist(y)) {
            if (nchar(current_line) + nchar(word) + 1 <= width) {
                if (current_line != "") {
                    current_line <- paste(current_line, "_", word, sep = "")
                } else {
                    current_line <- word
                }
            } else {
                lines <- c(lines, current_line)
                current_line <- word
            }
        }
        lines <- c(lines, current_line)
        return(lines)
    })
    return(lines)
}

