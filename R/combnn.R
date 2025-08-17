#' Get all element combinations of every lengths, except zero
#'
#' The empty element is not included.
#' Combinations -> subsets, order doesn’t matter, no repetition. This is binomial
#' coefficients.
#' Permutations -> subsets, order matters, no repetition. With factorial().
#' Cartesian product -> subsets, order matters, with repetition. This is expand.grid
#' of all lengths of x.
#'
#' @param x a vector
#'
#' @returns list
#' @export
#'
#' @examples
#' vec <- c("this", "is", "a", "character", "vector")
#' out <- combnn(vec)
combnn <- function(x,
                   order_matters = F,
                   repeats_allowed = F,
                   min_len = 1,
                   consecutive_only = F) {

    # Combinations -> subsets, order doesn’t matter, no repetition.
    # Permutations -> subsets, order matters, no repetition.
    # expand.grid() -> Cartesian product, order matters, repetition allowed. That means c(1,1) is valid, which is not what combn() or permutations().

    n <- length(x)
    s <- min_len # start
    seq <- s:n
    x_num <- seq_along(x)

    if (!order_matters) {
        # this is all 'combinations'

        # total: all but the empty element
        total <- as.integer(2^n - 1)

        # single binomial coefficients
        outcomes_by_size <- stats::setNames(choose(n, seq), as.character(seq))

        all_grouped <- purrr::map(stats::setNames(seq, as.character(seq)), ~utils::combn(x_num, .x, simplify = FALSE))

        if (consecutive_only) {
            diffs <- purrr::map(all_grouped, ~purrr::map(.x, diff))
            # set length 1 to diff 1
            ind <- which(names(diffs) == "1")
            if (length(ind)) {
                for (i in seq_along(diffs[[ind]])) {
                    diffs[[ind]][[i]] <- 1
                }
            }

            is_consecutive <- purrr::map(diffs, ~purrr::map_lgl(.x, ~all(dplyr::near(.x, 1))))
            all_grouped <- purrr::map2(all_grouped, is_consecutive, ~.x[which(.y)])
            all_grouped <- purrr::map(all_grouped, ~purrr::map(.x, ~x[.x]))
        }

        all_flat <- purrr::list_flatten(all_grouped)


        # if (consecutive_only) {
        #     is_consecutive <- purrr::map_lgl(purrr::map(all_flat, diff), ~all(dplyr::near(.x, 1)))
        #     all_flat <- all_flat[is_consecutive]
        # }
        # all_flat <- purrr::map(all_flat, ~x[.x])


    } else {
        # this is 'permutations of all possible lengths'

        outcomes_by_size <- purrr::map_int(stats::setNames(seq, as.character(seq)), ~factorial(n) / factorial(n - .x))
        total <- sum(outcomes_by_size)

        # with repeats allowed all_grouped is expand.grid(x), expand.grid(x,x), expand.grid(x,x,x), ....
        all_grouped <- purrr::map(stats::setNames(seq, as.character(seq)), ~gtools::permutations(n = n, r = .x, v = x, repeats.allowed = repeats_allowed))
        all_grouped <- purrr::map(all_grouped, asplit, MARGIN = 1)
        all_flat <- purrr::list_flatten(all_grouped)
    }

    return(list(
        total = total,
        grouped = outcomes_by_size,
        combs = all_grouped,
        combs2 = all_flat
    ))
}


