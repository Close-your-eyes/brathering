#' Get min indices with ties
#'
#' Instead of first index only as in which.min, all indices
#' corresponding to min(x) are returned.
#'
#' @param x numeric vector
#' @param na.rm remove NA?
#'
#' @return vector of indices
#' @export
#'
#' @examples
#' x <- c(1,5,2,5,1)
#' which.min(x)
#' which.min2(x)
which.min2 <- function(x, na.rm = F) {
    which(dplyr::near(x, min(x, na.rm = na.rm)))
}

#' Get max indices with ties
#'
#' Instead of first index only as in which.max, all indices
#' corresponding to max(x) are returned.
#'
#' @param x numeric vector
#' @param na.rm remove NA?
#'
#' @return vector of indices
#' @export
#'
#' @examples
#' x <- c(1,5,2,5,1)
#' which.max(x)
#' which.max2(x)
which.max2 <- function(x, na.rm = F) {
    which(dplyr::near(x, max(x, na.rm = na.rm)))
}
