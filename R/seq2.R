#' Vectorized version of seq
#'
#' @param from vector of start values
#' @param to vector of end values
#' @param algorithm two procedures to choose from, for education only,
#' use algorithm 1 which is quicker
#'
#' @return list of integer sequences
#' @export
#'
#' @examples
#' # algorithm 2 is much slower and needs much more memory
#' fun_alg1 <- seq2(c(1:1000), c(2001:3000), algorithm = 1)
#' fun_alg2 <- seq2(c(1:1000), c(2001:3000), algorithm = 2)
#' system.time(replicate(1000, fun_alg1))
#' system.time(replicate(10, fun_alg2))
seq2 <- function(from = 1, to = 1, algorithm = c("1","2")) {

    stopifnot("from and to need to have same length." = length(from) == length(to))

    algorithm <- as.character(algorithm)
    algorithm <- rlang::arg_match(algorithm)

    if (algorithm == "1") {
        x <- seq2_default(from = from, to = to)

        # make sure always a list is returned
        if (is.matrix(x)) {
            x <- unname(as.list(as.data.frame(x)))
        }
    }
    if (algorithm == "2") {
        x <- mapply("seq", from, to)
    }

    return(x)
}

seq2_default <- Vectorize(seq.default, vectorize.args = c("from", "to"))

