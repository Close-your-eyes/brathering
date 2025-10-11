#' Generate random variable name that does not appear in x
#'
#' E.g. for column name of data frame
#'
#' @param x character vector
#' @param prefix prefix to return
#' @param n_digits random digits
#' @param max_attempts loop rounds until n_digits increments by one digit
#'
#' @returns character
#' @export
#'
#' @examples
#' df <- data.frame(a=3,b=8,c=12)
#' df <- df %>% dplyr::mutate(!!random_varname(names(.)) := 4)
random_varname <- function(x,
                           prefix = "tmp_",
                           n_digits = 4,
                           max_attempts = 1000) {
    attempts <- 0
    while (TRUE) {
        # generate a random numeric suffix
        suffix <- paste0(sample(0:9, n_digits, replace = TRUE), collapse = "")
        name <- paste0(prefix, suffix)
        if (!(name %in% x)) {
            return(name)
        }
        # count attempts and expand the suffix length if needed
        attempts <- attempts + 1
        if (attempts >= max_attempts) {
            n_digits <- n_digits + 1
            attempts <- 0  # reset counter after expansion
            message(sprintf("increasing n_digits to %d", n_digits))
        }
    }
}
