#' Combine / average correlation coefficients
#'
#' Calculates Fisher z average: tanh(mean(atanh(x)))
#'
#' @param x vector of correlation coefficients
#'
#' @returns average coeff, numeric
#' @export
#'
#' @examples
combine_corrcoeff <- function(x) {

    x <- x[which(!is.na(x))]
    x <- as.numeric(x)
    # Fisher z average
    return(tanh(mean(atanh(x))))
}
