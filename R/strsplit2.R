#' Split character at pattern indices
#'
#' @param x character vector
#' @param pattern split pattern in x
#' @param inds indices of pattern found in x to use for splitting
#' @param SIMPLIFY from mapply, try to return matrix? only works with one inds
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- c("a_b_c_d_e_f", "q_w_e_r_t")
#' strsplit2(x, pattern = "_", c(1,2,5), SIMPLIFY = FALSE)
#' strsplit2(x, pattern = "_", 2, SIMPLIFY = TRUE)
strsplit2 <- function(x, pattern, inds = 1, SIMPLIFY = FALSE) {
    mapply(x = x, y = gregexpr(pattern, x), function(x,y) {

        if (length(inds) > 1) {
            # when splitting at more than one index, use different method
            li <- strsplit_at(x,y[inds])
            return(c(li[1], purrr::map_chr(li[-1], ~substr(.x, nchar(pattern)+1, nchar(.x)))))
        } else {
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

