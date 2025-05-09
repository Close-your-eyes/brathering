#' Split a vector at multiple indices
#'
#' @param x vector to split
#' @param pos vector of split indices
#'
#' @return
#' @export
#'
#' @examples
#' split_at(x = letters, pos = c(3,10,6))
split_at <- function(x, pos) {
    # maybe slow https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
    unname(split(x, cumsum(seq_along(x) %in% pos)))
}

