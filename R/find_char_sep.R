#' Title
#'
#' @param names
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
find_char_sep <- function(names, sep = "_") {
    while(any(sep %in% names)) {
        sep <- paste0(sep, sep)
    }
    return(sep)
}
