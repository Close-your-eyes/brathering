#' Split character at pattern indices
#'
#' @param x character vector
#' @param pattern split pattern in x
#' @param inds indices of pattern found in x to use for splitting;
#' provide "last" to split at last occurence
#' @param SIMPLIFY from mapply, try to return matrix? only works with one inds
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- c("a_b_c_d_e_f", "q_w_e_r_t")
#' strsplit2(x, pattern = "_", inds = c(1,2,5), SIMPLIFY = FALSE)
#' strsplit2(x, pattern = "_", inds = 2, SIMPLIFY = TRUE)
strsplit2 <- function(x, pattern, inds = 1, SIMPLIFY = FALSE) {

    mapply(x = x, y = gregexpr(pattern, x), function(x,y) {

        if (length(inds) > 1) {
            # when splitting at more than one index, use different method
            if ("last" %in% inds) {
                inds <- c(inds, length(y))
                inds <- inds[-which(inds == "last")]
            }
            inds <- sort(as.numeric(inds))
            li <- strsplit_at(x,y[inds])
            return(c(li[1], purrr::map_chr(li[-1], ~substr(.x, nchar(pattern)+1, nchar(.x)))))
        } else {
            if ("last" == inds) {
                inds <- c(inds, length(y))
                inds <- inds[-which(inds == "last")]
            }
            inds <- as.numeric(inds)
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
