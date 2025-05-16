#' Split string into chunks of length n
#'
#' @param x character
#' @param n chunk length
#' @param attach_last attach last chunk to second last if it is shorter than n
#' @param sep separator for rejoining, e.g. linebreak
#'
#' @return character vector
#' @export
#'
#' @examples
#' thanks_path <- file.path(R.home("doc"), "THANKS")
#' thanks <- stringr::str_c(readLines(thanks_path), collapse = "\n")
#' thanks <- stringr::word(thanks, 1, 3, stringr::fixed("\n\n"))
#' strsplit_n(x = thanks, n = 20, attach_last = TRUE)
strsplit_n <- function(x,
                       n,
                       attach_last = FALSE,
                       sep = NULL) {

    inds <- seq(1,nchar(x),n)
    chars <- strsplit(x, "")[[1]]

    spl <- split_at(chars, inds)
    spl <- purrr::map(spl, paste, collapse = "")

    if (attach_last) {
        if (nchar(spl[length(spl)]) < n) {
            spl[length(spl)-1] <- paste(spl[length(spl)-1], spl[length(spl)], collapse = "", sep = "")
            spl <- spl[-length(spl)]
        }
    }

    spl <- unlist(spl)
    if (!is.null(sep)) {
        spl <- paste(spl, collapse = sep)
    }

    return(spl)
}

