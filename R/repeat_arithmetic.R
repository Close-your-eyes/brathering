#' Title
#'
#' @param x
#' @param fun
#' @param y
#' @param times
#'
#' @return
#' @export
#'
#' @examples
repeat_arithmetic <- function(x,
                              y = 2,
                              times = 10,
                              fun = `/`) {

    stopifnot("y must be numeric." = is.numeric(y),
              "x must be numeric." = is.numeric(x),
              "times must be numeric." = is.numeric(times),
              "x, y, times must have length one." = all(lengths(list(x,y,times)) == 1))


    fun <- match.fun(fun)
    out <- numeric(times+1)
    out[1] <- x
    for (i in 1:times) {
        out[i+1] <- fun(out[i], y)
    }
    return(out)
}

