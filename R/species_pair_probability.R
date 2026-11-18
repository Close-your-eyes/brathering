#' Calculate probabilities for random pairs
#'
#' Calculates the probability of each unordered pair of types when two
#' individuals are drawn uniformly at random without replacement from
#' a population with known counts by type. One use case is estimating
#' how often each combination of cell types would occur among doublets
#' under random pairing.
#'
#' @param counts A named numeric vector of nonnegative whole-number
#'   counts, one per type. Names must be unique and nonempty, and the
#'   counts must sum to at least two.
#' @param mixed_only A logical value. If \code{TRUE}, calculate
#'   probabilities conditional on the two individuals being of different
#'   types. This requires at least two types with positive counts. The
#'   default, \code{FALSE}, includes same-type pairs.
#'
#' @return A data frame with one row per eligible pair, including pairs
#'   with zero probability. The \code{species_1} and \code{species_2}
#'   columns contain the type labels. The \code{probability} column gives
#'   the unrounded probability, and \code{percent} gives that probability
#'   as a percentage rounded to two decimal places.
#'
#' @examples
#' # statistical name: multivariate hypergeometrical distribution
#' counts <- c(A = 30, B = 25, C = 45)
#' species_pair_probabilities(counts)
#' species_pair_probabilities(counts, mixed_only = TRUE)
#'
#' # For one pair chosen in advance, compare its observed count with the expected
#' # count. For example, if the model predicts 15% A–B doublets and you observe 100
#' # doublets, you’d expect about 15 A–B doublets by chance. If the doublets are
#' # approximately independent and the baseline probability is well established, use
#' # a one-sided test in R: binom.test(observed_AB, total_doublets, p =
#' # expected_AB_probability, alternative = "greater"). Use the probability column
#' # from the function for p.
#'
#' @export
species_pair_probabilities <- function(counts, mixed_only = FALSE) {

    species <- names(counts)
    if (is.null(species) || anyNA(species) ||
        any(!nzchar(species)) || anyDuplicated(species) > 0L) {
        stop("Give each count a unique, nonempty species name.")
    }

    if (!is.numeric(counts) || !is.null(dim(counts)) ||
        any(!is.finite(counts)) || any(counts < 0) ||
        any(counts != floor(counts))) {
        stop("Species counts must be nonnegative whole numbers.")
    }

    total <- sum(counts)
    if (total < 2) {
        stop("The population must contain at least two individuals.")
    }

    # Count pairs between different species and within each species.
    pair_counts <- outer(counts, counts, "*")
    diag(pair_counts) <- choose(counts, 2)

    pairs <- which(upper.tri(pair_counts, diag = !mixed_only), arr.ind = TRUE)
    eligible_counts <- pair_counts[pairs]
    total_eligible_pairs <- sum(eligible_counts)

    if (total_eligible_pairs == 0) {
        stop("Mixed-type pairs require at least two cell types with positive counts.")
    }

    probability <- eligible_counts / total_eligible_pairs

    data.frame(
        species_1 = species[pairs[, 1]],
        species_2 = species[pairs[, 2]],
        probability = probability,
        percent = round(100 * probability, 2)
    )
}
