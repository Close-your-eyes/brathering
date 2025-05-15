#' Rounding while preserving sum of elements
#'
#' @param x numeric vector
#' @param digits decimal places to round to
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' x <- c(21.5539062420449, 19.7605304961694, 20.6575217766391, 20.3202590252772,
#' 20.1906342102765, 21.5574387808329, 19.7095713897403, 19.4116620736685,
#' 20.6269265808014, 21.3010177224109)
#' sum(x)
#' sum(round(x,1))
#' sum(round2(x,1))
round2 <- function(x, digits = 0) {
    #https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum
    up <- 10 ^ digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
}
