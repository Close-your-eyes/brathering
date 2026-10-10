#' Plot a hierarchical clustering dendrogram
#'
#' Creates a horizontal dendrogram from a correlation matrix or distance data.
#' Supply exactly one of `corrmat` or `distmat`.
#'
#' @param corrmat A square numeric correlation matrix containing at least two
#'   objects. Values must be finite and between -1 and 1, with a diagonal of 1.
#' @param distmat A square numeric distance matrix or an object of class `dist`.
#'   Distances must be finite and nonnegative, with a diagonal of 0 and at least
#'   two objects.
#' @param hclust_args A named list of additional arguments passed to
#'   [stats::hclust()]. Supported entries are `method` and `members`.
#'   Defaults to an empty list, which uses complete linkage.
#' @param label_size Numeric text size passed to [ggplot2::geom_text()].
#'   Defaults to 3.
#'
#' @details
#' Correlations are converted to dissimilarities by subtracting each
#' correlation from one.
#'
#' Matrix inputs must be symmetric. If row and column names are supplied,
#' they must match. Validation allows numerical differences within a tolerance
#' of `1e-8`. Accepted inputs are symmetrized, negligible negative distances
#' are set to zero, and diagonal distances are set to zero before clustering.
#'
#' Invalid inputs produce an error.
#'
#' @return A named list containing:
#' \describe{
#'   \item{hc}{An object of class `hclust` containing the clustering result.}
#'   \item{plot}{A `ggplot` object containing the horizontal dendrogram.}
#' }
#'
#' @examples
#' result <- dendro_plot(corrmat = stats::cor(USArrests))
#' print(result$plot)
#'
#' result <- dendro_plot(
#'   distmat = stats::dist(scale(USArrests)),
#'   hclust_args = list(method = "average"),
#'   label_size = 2
#' )
#' print(result$plot)
#'
#' @seealso [stats::hclust()], [stats::as.dist()],
#'   [ggdendro::dendro_data()]
#' @export
dendro_plot <- function(corrmat = NULL,
                        distmat = NULL,
                        hclust_args = list(),
                        label_size = 3) {
    fail <- function(message) stop(message, call. = FALSE)
    tol <- 1e-8

    if (is.null(corrmat) == is.null(distmat)) {
        fail("Supply exactly one of `corrmat` or `distmat`.")
    }

    if (!is.list(hclust_args)) {
        fail("`hclust_args` must be a list.")
    }

    if (length(hclust_args)) {
        arg_names <- names(hclust_args)

        if (is.null(arg_names) ||
            anyNA(arg_names) ||
            anyDuplicated(arg_names) ||
            !all(arg_names %in% c("method", "members"))) {
            fail("`hclust_args` may contain only named `method` and `members` entries.")
        }
    }

    is_corr <- !is.null(corrmat)
    mat <- as.matrix(if (is_corr) corrmat else distmat)

    if (!is.numeric(mat) ||
        nrow(mat) != ncol(mat) ||
        nrow(mat) < 2L) {
        fail("Input must be a square numeric matrix with at least two objects.")
    }

    if (any(!is.finite(mat))) {
        fail("Input must not contain NA, NaN, or infinite values.")
    }

    if (!isSymmetric(mat, tol = tol)) {
        fail("Input must be symmetric, with matching row and column names.")
    }

    if (is_corr) {
        if (any(abs(mat) > 1 + tol) ||
            any(abs(diag(mat) - 1) > tol)) {
            fail("Correlations must be between -1 and 1, with a diagonal of 1.")
        }

        mat <- 1 - mat
    } else {
        if (any(mat < -tol) || any(abs(diag(mat)) > tol)) {
            fail("Distances must be nonnegative, with a diagonal of 0.")
        }
    }

    # Normalize negligible floating-point differences.
    mat <- (mat + t(mat)) / 2
    mat[mat < 0] <- 0
    diag(mat) <- 0

    hc <- do.call(
        stats::hclust,
        c(list(d = stats::as.dist(mat)), hclust_args)
    )

    hcdata <- ggdendro::dendro_data(hc, type = "rectangle")

    dendroplot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
            data = ggdendro::segment(hcdata),
            mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
        ) +
        ggplot2::geom_text(
            data = ggdendro::label(hcdata),
            mapping = ggplot2::aes(x = x, y = y, label = label),
            hjust = 0,
            size = label_size
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_reverse(expand = c(0.2, 0)) +
        ggplot2::theme_void()

    list(hc = hc, plot = dendroplot)
}
