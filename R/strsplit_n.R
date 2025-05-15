#' Title
#'
#' @param x
#' @param n
#' @param attach_last
#' @param sep separator for rejoining, e.g. linebreak
#'
#' @return
#' @export
#'
#' @examples
strsplit_n <- function(x,
                       n,
                       attach_last = F,
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
#?stringr::str_wrap()
# thanks_path <- file.path(R.home("doc"), "THANKS")
# thanks <- str_c(readLines(thanks_path), collapse = "\n")
# thanks <- word(thanks, 1, 3, fixed("\n\n"))
# strsplit_n(x = thanks, n = 20, attach_last = T, insert_seps = F)



