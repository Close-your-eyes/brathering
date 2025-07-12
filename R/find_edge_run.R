#' Find stretches of leading or trailing sequences below a threshold
#'
#' @param x numeric vector
#' @param threshold upper threshold
#' @param fun comparison between x and threshold, x comes first: x < threshold
#'
#' @return named numeric vector
#' @export
#'
#' @examples
#' x <- c(0.5, 1.2, 2.1, 3.5, 0.8, 0.3, 4.2, 5.1, 0.1, 0.2)
#' find_edge_run_indices(x, threshold = 1)
#' x <- c(NA,NA, 0.5, 1.2, 2.1, 3.5, 0.8, 0.3, 4.2, 5.1,1, 0.1, 0.2,NA)
#' find_edge_run_indices(x, threshold = 6, fun = "<")
#' #' x <- c(NA,NA, 0.5, 1.2, 2.1, 3.5, 0.8, 0.3, 4.2, 5.1,1, 0.1, 0.2,NA)
#' find_edge_run_indices(x, threshold = 5, fun = "<")
#' x <- c(NA,NA, 0.5, 1.2, 2.1, 3.5, NA, 0.8, 0.3, 4.2, 5.1,1, 0.1, 0.2,NA)
#' find_edge_run_indices(x, threshold = 5, fun = "<")
find_edge_run_indices <- function(x, threshold, fun = "<") {
    fun <- match.fun(fun)
    n <- length(x)

    # 1. locate the “real” data (first/last non-NA)
    non_na <- which(!is.na(x))
    if (length(non_na) == 0) {
        # nothing but NAs
        return(list(leading = integer(0), trailing = integer(0)))
    }
    first_data <- non_na[1]
    last_data  <- non_na[length(non_na)]

    # 2. build logical run of “below threshold”, treating NA as FALSE
    below <- fun(x, threshold)
    below[is.na(below)] <- FALSE
    runs <- rle(below)
    lens <- runs$lengths
    ends <- cumsum(lens)
    starts <- ends - lens + 1

    # helper to find which run contains a given index i
    run_at <- function(i) which(starts <= i & ends >= i)

    # 3. leading
    if (x[first_data] < threshold) {
        j <- run_at(first_data)
        leading_idx <- seq.int(starts[j], ends[j])
    } else {
        leading_idx <- integer(0)
    }

    # 4. trailing
    if (x[last_data] < threshold) {
        j <- run_at(last_data)
        trailing_idx <- seq.int(starts[j], ends[j])
    } else {
        trailing_idx <- integer(0)
    }

    return(list(leading = leading_idx, trailing = trailing_idx))
}

