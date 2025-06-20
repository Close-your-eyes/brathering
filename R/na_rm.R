#' Remove NA from vector and record their indices
#'
#' This may allow to work with the 'cleaned' vector x and subsequently
#' re-insert the NA.
#'
#' @param x vector with NA
#' @param replace NULL to remove NA, or any value to replace them with
#'
#' @return vector
#' @export
#'
#' @examples
#' x_wo_na <- na_rm(x = c(NA,1,2,NA,3,4,5,NA))
#' x_wo_na
#' x_restored <- na_insert(x_wo_na)
#' x_restored
na_rm <- function(x, replace = NULL) {
    na_inds <- which(is.na(x))

    if (!is.null(replace) && length(na_inds)) {
        x[na_inds] <- replace

    } else {
        x <- x[!is.na(x)]
    }
    return(list(x = x, na_inds = na_inds))
}

#' Insert NA at specific indices
#'
#' Works on its own but especially in concert with brathering::na_rm.
#'
#' @param x a vector or a list as returned from brathering::na_rm
#' @param na_inds indices were NA should be inserted in final vector
#'
#' @return vector
#' @export
#'
#' @examples
#' x_wo_na <- na_rm(x = c(NA,1,2,NA,3,4,5,NA))
#' x_wo_na
#' x_restored <- na_insert(x_wo_na)
#' x_restored
#' # na_inds can be tricky as length(x) increases
#' na_insert(x = c(1:5), na_inds = c(1,3,5,9))
na_insert <- function(x, na_inds = NULL) {
    if (is.list(x) && all(names(x) %in% c("x", "na_inds"))) {
        na_inds <- x[["na_inds"]]
        x <- x[["x"]]
    }
    if (is.null(na_inds) || !length(na_inds)) {
        return(x)
    }
    orig_len <- length(x) + length(na_inds)
    x2 <- rep(NA, orig_len)
    x2[-na_inds] <- x
    return(x2)
}

#' Alter a vector of indices according to inserted NA
#'
#' The resulting vector of indices points to the same values as before
#' insertion of NA.
#'
#' @param x vector before addition of NA
#' @param x_na vector after addition of NA
#' @param x_inds vector of original indices pointing to desired values in x
#'
#' @return adjusted indices vector
#' @export
#'
#' @examples
#' # initial vector may contain NA
#' x <- c(NA,1:10)
#' x_inds <- seq(1,10,2)
#' na_inds <- c(2,4,6)
#' # orignal values
#' x[x_inds]
#' # insert additional NA
#' x2 <- na_insert(x, na_inds)
#' x_inds2 <- na_fix_ind(x, x2, x_inds)
#' x2[x_inds2] # same as x[x_inds]
na_fix_ind <- function(x, x_na, x_inds) {
    # Match positions of original elements (including NAs) within modified vector
    orig_pointer <- rep(NA_integer_, length(x))

    orig_idx <- 1
    for (i in seq_along(x_na)) {
        if (orig_idx > length(x)) break
        if (identical(x_na[[i]], x[[orig_idx]])) {
            orig_pointer[orig_idx] <- i
            orig_idx <- orig_idx + 1
        }
    }

    adj_inds <- orig_pointer[x_inds]
    return(adj_inds)
}
