#' Split string at multiple indices
#'
#' @param x a character
#' @param pos split indices
#'
#' @return vector of characters
#' @export
#'
#' @examples
#' strsplit_at("Hello my name is", c(2,5,7))
strsplit_at <- function(x, pos) {
    unlist(purrr::map(split_at(x = strsplit(x = x, "")[[1]], pos = pos), paste, collapse = ""))
}
