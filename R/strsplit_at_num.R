#' Split string at boundaries of numeric and non-numeric
#'
#' First step to extraction or modification of numerical string parts
#'
#' @param x character vector
#'
#' @returns list of split characters
#' @export
#'
#' @examples
#' strsplit_at_num(x = c("abc45efg690hij5xyz))
strsplit_at_num <- function(x) {
    # (?<=\\d)(?=\\D) matches the boundary between a digit and a non-digit.
    # (?<=\\D)(?=\\d) matches the boundary between a non-digit and a digit.
    # | joins them so it works in both directions.
    # perl = TRUE enables lookaround regex.
    strsplit(x, "(?<=\\d)(?=\\D)|(?<=\\D)(?=\\d)", perl = TRUE)
}
