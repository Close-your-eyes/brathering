#' Highly efficient pairwise jaccard index
#'
#' @param x list of character vectors
#' @param as how to return
#'
#' @returns matrix
#' @export
#'
#' @examples
jaccard_pairwise <- function(x,
                             as = c("sparse", "dense")) {

    as <- rlang::arg_match(as)
    y <- intersect_pairwise(x, as = as)
    rs <- lengths(x)
    jaccard <- y / (outer(rs, rs, "+") - y) # unions <- outer(rs, rs, "+") - intersections

    return(jaccard)
}
