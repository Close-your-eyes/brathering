#' Floor to a given decimal place
#'
#' @param x numeric vector
#' @param accuracy decimal accuracy; as decimal like 0.1 or 0.01 or as digits
#' like 1, 2 as in base::round()
#' @param fun function
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' floor2(rnorm(20))
floor2 <- function(x,
                   accuracy = 1,
                   fun = base::floor) {

    if (accuracy > 0) {
        accuracy <- 10^-accuracy
    }

    fun(x/ accuracy) * accuracy
}

#' Ceiling to a given decimal place
#'
#' @param x numeric vector
#' @param accuracy decimal accuracy; as decimal like 0.1 or 0.01 or as digits
#' like 1, 2 as in base::round()
#' @param fun function
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' ceiling2(rnorm(20))
ceiling2 <- function(x,
                     accuracy = 1,
                     fun = base::ceiling) {

    if (accuracy > 0) {
        accuracy <- 10^-accuracy
    }
    fun(x/ accuracy) * accuracy
}



#' Adapt decimal places to input values
#'
#' Largest absolute value above 100: 0 digits.
#' Above 1: 1 digit.
#' Below 1: (largest decimal place with differences) + 1
#'
#' @param x numeric vector
#'
#' @returns integer of suggested meaningful decimal places to round to
#' @export
#'
#' @examples
#' decimals_adaptive(c(109,15))
#' decimals_adaptive(x = c(10.5,15))
#' decimals_adaptive(x = c(10,15))
#' decimals_adaptive(x = c(1.5,5))
#' decimals_adaptive(x = c(1,5))
#' decimals_adaptive(x = c(0.4,0.2))
#' y <- decimals_adaptive(x = c(0.456,0.265))
#' # use for rounding
#' z <- round(x = c(0.456,0.265), digits = y)
decimals_adaptive <- function(x) {

    if (all(is.na(x))) return(0)
    if (all(x == 0)) return(0)


    max_val <- max(abs(x), na.rm = T)

    if (max_val > 100) return(0)

    if (max_val > 1 && any(abs(x - round(x)) > .Machine$double.eps^0.5)) {
        return(1)
    } else if (max_val > 1) {
        return(0)
    }

    # decimals <- max(0, -floor(log10(max_val))) + 1
    # if (all(abs(x - round(x, decimals)) < .Machine$double.eps^0.5)) {
    #     decimals <- decimals - 1
    # }

    decimals <- meaningful_decimals(x) + 1
    # +1 as rounding may cause differences in a decimal place that is yet equal

    # rm one dec place if only zeros follow
    if (all(abs(x - round(x, decimals)) < .Machine$double.eps^0.5)) {
        decimals <- decimals - 1
    }

    return(decimals)
}


meaningful_decimals <- function(x, max_decimals = 15) {
    # Remove NA
    x <- stats::na.omit(x)

    if (length(unique(x)) <= 1) return(0)  # all same

    for (d in 0:max_decimals) {
        if (length(unique(round(x, d))) == length(unique(x))) {
            return(d)
        }
    }
    return(max_decimals)
}

