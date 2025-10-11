#' Find a valid separator which is not present in any provided character
#'
#' Sep is repeated until it is unique (not found in any of x).
#'
#' @param x character vector
#' @param sep character
#'
#' @return character
#' @export
#'
#' @examples
#' x <- c("my_name_is", "coward__harpendale")
#' find_sep(x = x)
find_sep <- function(x, sep = "_") {
    while(any(grepl(sep, x))) {
        sep <- paste0(sep, sep)
    }
    return(sep)
}
