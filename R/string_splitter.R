#' Split character at pattern indices
#'
#' @param x character vector
#' @param pattern split pattern in x
#' @param inds indices of pattern found in x to use for splitting
#' @param SIMPLIFY from mapplay, try to return matrix? only works with one inds
#'
#' @return
#' @export
#'
#' @examples
#' x <- c("a_b_c_d_e_f", "q_w_e_r_t")
#' strsplit2(x, pattern = "_", c(1,2,5), SIMPLIFY = F)
#' strsplit2(x, pattern = "_", 2, SIMPLIFY = T)
strsplit2 <- function(x, pattern, inds = 1, SIMPLIFY = F) {
    mapply(x = x, y = gregexpr(pattern, x), function(x,y) {

        if (length(inds) > 1) {
            li <- strsplit_at(x,y[inds])
            return(c(li[1], purrr::map_chr(li[-1], ~substr(.x, nchar(pattern)+1, nchar(.x)))))
        } else {
            if (inds > length(y)) {
                return(x)
            } else {
                return(c(substr(x, 1, y[inds]-1),
                         substr(x, y[inds]+1, nchar(x))))
            }
        }

    }, SIMPLIFY = SIMPLIFY)
}

