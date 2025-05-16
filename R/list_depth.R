#' Get levels of nesting
#'
#' @param x a nested list
#'
#' @return numeric
#' @export
#'
#' @examples
#' list_depth(list(z = list(y = list(x = 3))))
list_depth <- function(x) {
    if (!is.list(x)) {
        return(0)
    } else if (length(x) == 0) {
        return(1)
    } else {
        return(1 + max(sapply(x, list_depth)))
    }
}


