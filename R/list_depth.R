#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
list_depth <- function(x) {
    if (!is.list(x)) {
        return(0)
    } else if (length(x) == 0) {
        return(1)
    } else {
        return(1 + max(sapply(x, list_depth)))
    }
}
