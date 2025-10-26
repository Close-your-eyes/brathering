#' Average p-values with fisher of stouffer method
#'
#' Check packages metap or poolr
#'
#' @param pvalues vector of p-values
#' @param method method
#' @param sample_sizes for stouffer method:
#' optional samples size to add weights to p-vals
#'
#' @returns numeric: averaged p-value
#' @export
#'
#' @examples
combine_pvalues <- function(pvalues,
                            method = c("fisher", "stouffer"),
                            sample_sizes = NULL) {
    # check
    # install.packages("metap")
    # install.packages("poolr")

    # Input checks
    method <- rlang::arg_match(method)
    pvalues <- as.numeric(pvalues)

    if (any(pvalues <= 0 | pvalues > 1, na.rm = TRUE)) {
        stop("All p-values must be between 0 and 1 (exclusive).")
    }

    if (method == "fisher") {
        # Fisher's method: Chi-square combination
        k <- length(pvalues)
        chi_stat <- -2 * sum(log(pvalues), na.rm = TRUE)
        p_combined <- pchisq(chi_stat, df = 2 * k, lower.tail = FALSE)

    } else if (method == "stouffer") {
        # Stouffer's method: combine z-scores (optionally weighted)
        z_scores <- qnorm(1 - pvalues / 2)  # two-sided to z-scores

        if (is.null(sample_sizes)) {
            z_combined <- mean(z_scores, na.rm = TRUE) / sqrt(1 / length(z_scores))
        } else {
            sample_sizes <- as.numeric(sample_sizes)
            if (length(sample_sizes) != length(pvalues)) {
                stop("Length of sample_sizes must match length of p-values.")
            }
            weights <- sqrt(sample_sizes)
            z_combined <- sum(weights * z_scores, na.rm = TRUE) / sqrt(sum(weights^2, na.rm = TRUE))
        }

        # back to two-sided p-value
        p_combined <- 2 * (1 - pnorm(abs(z_combined)))
    }

    return(p_combined)
}
