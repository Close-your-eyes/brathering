#' Find Best String Matches from a Candidate Vector
#'
#' Computes pairwise string distances between a character vector and a set of
#' candidate strings using `stringdist::stringdistmatrix()`, then returns the
#' closest match for each input string.
#'
#' @param x A character vector of strings to match.
#' @param candidates A character vector of candidate strings against which
#'   matches are evaluated.
#' @param method String distance method passed to
#'   [stringdist::stringdistmatrix()]. Defaults to `"osa"`.
#'
#' @returns A list with two elements:
#' \describe{
#'   \item{result}{A data frame containing the original query string,
#'   the best matching candidate, and the corresponding string distance.}
#'   \item{best_match}{A named character vector of best matches, with names
#'   corresponding to the input values in `x`.}
#' }
#'
#' @export
#'
#' @examples
#' queries <- c("appel", "bananna", "oragne")
#' candidates <- c("apple", "banana", "orange", "pear")
#'
#' out <- best_match_str(queries, candidates)
#'
#' out$result
#' out$best_match
best_match_str <- function(x, candidates, method = "osa") {
    d <- stringdist::stringdistmatrix(x, candidates, method = method)

    idx <- max.col(-d, ties.method = "first")

    result <- data.frame(
        query = x,
        best_match = candidates[idx],
        distance = d[cbind(seq_along(idx), idx)],
        stringsAsFactors = FALSE
    )

    res <- list(
        result = result,
        best_match = setNames(result$best_match, result$query)
    )

    return(res)
}

