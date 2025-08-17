#' Split character at pattern indices
#'
#' @param x character vector
#' @param pattern split pattern in x
#' @param inds indices of pattern found in x to use for splitting;
#' use negative indices to indicate index from end
#' @param SIMPLIFY from mapply, try to return matrix? only works with one inds
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- c("a_b_c_d_e_f", "q_w_e_r_t")
#' strsplit2(x, pattern = "_", inds = c(1,2,-1), SIMPLIFY = FALSE)
#' strsplit2(x, pattern = "_", inds = 2, SIMPLIFY = TRUE)
strsplit2 <- function(x, pattern, inds = 1, SIMPLIFY = FALSE) {

    inds <- as.numeric(inds)
    inds <- inds[which(!is.na(inds))]
    inds <- inds[which(inds!=0)]
    mapply(x = x, y = gregexpr(pattern, x), function(x,y) {

        if (length(inds) > 1) {

            if (any(inds < 0)) {
                temp <- strsplit(x = x, split = pattern)[[1]]
                inds[which(inds<0)] <- length(temp) + inds[which(inds<0)]
            }

            inds <- sort(unique(inds))
            li <- strsplit_at(x,y[inds])

            return(c(li[1], purrr::map_chr(li[-1], ~substr(.x, nchar(pattern)+1, nchar(.x)))))
        } else {
            if (inds < 0) {
                temp <- strsplit(x = x, split = pattern)[[1]]
                inds <- length(temp) + inds
            }

            # split at one index only
            if (inds > length(y)) {
                # index not present in this string (x)
                return(x)
            } else {
                return(c(substr(x, 1, y[inds]-1),
                         substr(x, y[inds]+1, nchar(x))))
            }
        }

    }, SIMPLIFY = SIMPLIFY)
}


