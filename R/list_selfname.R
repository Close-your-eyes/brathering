#' Title
#'
#' @param lst
#'
#' @returns
#' @export
#'
#' @examples
list_selfname <- function(lst) {
browser()
    if (!is.null(names(lst))) {
        return(lst)
    }
    nme <- as.character(deparse(substitute(lst)))
    names(lst) <- strsplit(gsub("list\\(|\\)", "", nme), ", ")[[1]]
    return(lst)
}
