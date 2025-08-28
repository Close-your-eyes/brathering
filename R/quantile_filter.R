#' Filter a data frame by quantile limits
#'
#' @param df data frame
#' @param valuecol numeric column where to apply quantiles and filter on
#' @param groupcol optional grouping column
#' @param quantiles numeric of length 2 telling lower and upper quantile limit
#'
#' @returns data frame
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(1000))
#' # rm lower 10 % and upper 20 %
#' quantile_filter(df = df, valuecol = "x", quantiles = c(0.1, 0.8))
quantile_filter <- function(df,
                            valuecol,
                            groupcol = NULL,
                            quantiles = c(0,1)) {

    ## no checking of arguments

    ## FYI, this is inverse of quantile, empiric:
    # ecdfx <- ecdf(df[[valuecol]])   # empirical CDF function
    # ecdfx(3434) # which quantile does a value correspond to

    dplyr::filter(df,
                  dplyr::between(
                      x = !!rlang::sym(valuecol),
                      left = quantile(!!rlang::sym(valuecol), min(quantiles)),
                      right = quantile(!!rlang::sym(valuecol), max(quantiles))),
                  .by = groupcol)
}
