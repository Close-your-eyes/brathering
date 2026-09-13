#' Generate combinations or permutations of a vector
#'
#' Generates selections of `x` for every length from `min_len` through
#' `length(x)`. The empty selection is never included.
#'
#' @details
#' The arguments define four kinds of selection:
#'
#' * `order_matters = FALSE`, `repeats_allowed = FALSE`: combinations.
#' * `order_matters = FALSE`, `repeats_allowed = TRUE`: combinations with
#'   replacement.
#' * `order_matters = TRUE`, `repeats_allowed = FALSE`: permutations.
#' * `order_matters = TRUE`, `repeats_allowed = TRUE`: ordered selections with
#'   replacement (Cartesian powers).
#'
#' When `consecutive_only = TRUE`, each selected position must be exactly one
#' greater than the preceding position. Thus, selections longer than one are
#' forward, nonrepeated runs from `x`. Single-element selections are considered
#' consecutive.
#'
#' Selections are generated from positions in `x`. Consequently, duplicate
#' values at different positions are treated as distinct elements.
#'
#' @param x A one-dimensional atomic vector.
#' @param order_matters A single logical value. If `FALSE`, generate
#'   combinations; if `TRUE`, generate permutations.
#' @param repeats_allowed A single logical value indicating whether the same
#'   position in `x` may be selected more than once.
#' @param min_len A positive integer giving the minimum selection length. If
#'   greater than `length(x)`, an empty list is returned.
#' @param consecutive_only A single logical value. If `TRUE`, retain only
#'   selections whose positions form a forward consecutive run.
#' @param return_numeric A single logical value. If `FALSE`, return values from
#'   `x`. If `TRUE`, return their one-based positions in `x`.
#' @param max_results A positive integer giving the maximum number of selections
#'   that may be generated. This protects against accidental excessive memory
#'   use. Use `Inf` to disable the limit.
#'
#' @return
#' A named list grouped by selection length. Each group is a list of vectors.
#' The vectors contain values from `x`, or integer positions when
#' `return_numeric = TRUE`.
#'
#' @export
#'
#' @examples
#' vec <- c("this", "is", "a", "character", "vector")
#'
#' # Combinations without repetition
#' combnn(vec, min_len = 4L)
#'
#' # Permutations represented by positions
#' combnn(
#'     vec,
#'     order_matters = TRUE,
#'     min_len = 4L,
#'     return_numeric = TRUE
#' )
#'
#' # Forward consecutive runs
#' combnn(vec, min_len = 2L, consecutive_only = TRUE)
#'
#' # Combinations with replacement
#' combnn(letters[1:3], repeats_allowed = TRUE)
combnn <- function(x,
                   order_matters = FALSE,
                   repeats_allowed = FALSE,
                   min_len = 1L,
                   consecutive_only = FALSE,
                   return_numeric = FALSE,
                   max_results = 1e6) {

    brathering:::.ensure_packages(c("gtools"))

    # Combinations -> subsets, order doesn’t matter, no repetition.
    # Permutations -> subsets, order matters, no repetition.
    # expand.grid() -> Cartesian product, order matters, repetition allowed. That means c(1,1) is valid, which is not what combn() or permutations().

    logical_args <- list(
        order_matters = order_matters,
        repeats_allowed = repeats_allowed,
        consecutive_only = consecutive_only,
        return_numeric = return_numeric
    )

    valid_logical <- vapply(
        logical_args,
        function(value) {
            is.logical(value) &&
                length(value) == 1L &&
                !is.na(value)
        },
        logical(1)
    )

    if (any(!valid_logical)) {
        invalid <- names(logical_args)[!valid_logical]

        stop(
            sprintf(
                "%s must be a single non-missing logical value.",
                paste(sprintf("`%s`", invalid), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    if (!is.atomic(x) || !is.null(dim(x))) {
        stop("`x` must be a one-dimensional atomic vector.", call. = FALSE)
    }

    valid_min_len <- is.numeric(min_len) &&
        length(min_len) == 1L &&
        !is.na(min_len) &&
        is.finite(min_len) &&
        min_len >= 1 &&
        min_len == floor(min_len) &&
        min_len <= .Machine$integer.max

    if (!valid_min_len) {
        stop("`min_len` must be a positive integer.", call. = FALSE)
    }

    valid_max_results <- is.numeric(max_results) &&
        length(max_results) == 1L &&
        !is.na(max_results) &&
        max_results > 0 &&
        (is.infinite(max_results) ||
             max_results == floor(max_results))

    if (!valid_max_results) {
        stop(
            "`max_results` must be a positive integer or `Inf`.",
            call. = FALSE
        )
    }

    min_len <- as.integer(min_len)
    n <- length(x)

    if (n == 0L || min_len > n) {
        return(list())
    }

    sizes <- seq.int(min_len, n)

    candidate_counts <- if (consecutive_only) {
        as.double(n - sizes + 1L)
    } else if (order_matters && repeats_allowed) {
        n^sizes
    } else if (order_matters) {
        exp(lgamma(n + 1) - lgamma(n - sizes + 1))
    } else if (repeats_allowed) {
        choose(n + sizes - 1, sizes)
    } else {
        choose(n, sizes)
    }

    total_candidates <- sum(candidate_counts)

    too_many <- !is.infinite(max_results) &&
        (!is.finite(total_candidates) ||
             total_candidates > max_results)

    if (too_many) {
        stop(
            sprintf(
                paste0(
                    "This request would generate %s selections, exceeding ",
                    "`max_results = %s`. Increase `max_results` or use `Inf` ",
                    "to disable the safeguard."
                ),
                format(total_candidates, scientific = TRUE, trim = TRUE),
                format(max_results, scientific = FALSE, trim = TRUE)
            ),
            call. = FALSE
        )
    }

    positions <- seq_along(x)

    if (consecutive_only) {
        all_grouped <- lapply(sizes, function(size) {
            starts <- seq_len(n - size + 1L)

            lapply(starts, function(start) {
                seq.int(from = start, length.out = size)
            })
        })
    } else {
        if (!requireNamespace("gtools", quietly = TRUE)) {
            stop(
                "Package `gtools` is required. Install it with install.packages(\"gtools\").",
                call. = FALSE
            )
        }

        generator <- if (order_matters) {
            gtools::permutations
        } else {
            gtools::combinations
        }

        all_grouped <- lapply(sizes, function(size) {
            generated <- generator(
                n = n,
                r = size,
                v = positions,
                repeats.allowed = repeats_allowed
            )

            lapply(seq_len(nrow(generated)), function(row) {
                unname(as.integer(generated[row, , drop = TRUE]))
            })
        })
    }

    names(all_grouped) <- as.character(sizes)

    if (!return_numeric) {
        all_grouped <- lapply(all_grouped, function(group) {
            values <- lapply(group, function(index) x[index])

            labels <- vapply(
                values,
                function(value) paste(as.character(value), collapse = "_"),
                character(1)
            )

            names(values) <- make.unique(labels)
            values
        })
    }

    all_grouped
}

