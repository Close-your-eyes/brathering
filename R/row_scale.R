#' Center and scale matrix rows
#'
#' @param x A numeric matrix or Matrix object.
#' @param center Whether to subtract each row mean.
#' @param scale Whether to divide by each row's sample standard deviation.
#' @param add_attr Whether to attach scaling parameters.
#' @param rows Optional row indices.
#' @param cols Optional column indices.
#' @param drop.na.rows Whether to remove rows that are entirely NA/NaN
#'   after scaling.
#'
#' @return A dense numeric matrix.
#' @export
row_scale <- function(
        x,
        center = TRUE,
        scale = TRUE,
        add_attr = TRUE,
        rows = NULL,
        cols = NULL,
        drop.na.rows = TRUE
) {
    if (!is.null(rows) && !is.null(cols)) {
        x <- x[rows, cols, drop = FALSE]
    } else if (!is.null(rows)) {
        x <- x[rows, , drop = FALSE]
    } else if (!is.null(cols)) {
        x <- x[, cols, drop = FALSE]
    }

    x <- methods::as(
        Matrix::Matrix(x, sparse = TRUE),
        "dgCMatrix"
    )

    brathering::row_scale_dgC(
        x,
        center = center,
        scale = scale,
        add_attr = add_attr,
        drop_na_rows = drop.na.rows
    )
}
#
# row_scale = function(x,
#                      center = TRUE,
#                      scale = TRUE,
#                      add_attr = TRUE,
#                      rows = NULL,
#                      cols = NULL,
#                      drop.na.rows = T) {
#
#     ## redo this in c++
#     ## sparse mat!
#
#     if (!is.null(rows) && !is.null(cols)) {
#         x <- x[rows, cols, drop = FALSE]
#     } else if (!is.null(rows)) {
#         x <- x[rows, , drop = FALSE]
#     } else if (!is.null(cols)) {
#         x <- x[, cols, drop = FALSE]
#     }
#
#     rm = Matrix::rowMeans(x, na.rm = TRUE)
#     if (scale) {
#         csd = apply(x, 1, stats::sd)
#         #csd = matrixStats::rowSds(x, center = rm)
#     } else {
#         csd = rep(1, length = length(rm))
#     }
#     if (!center) {
#         rm = rep(0, length = length(rm))
#     }
#     x = (x - rm) / csd
#
#     if (drop.na.rows) {
#         x <- x[which(Matrix::rowSums(is.na(x))<ncol(x)),,drop=F]
#     }
#
#     if (add_attr) {
#         if (center) {
#             attr(x, "scaled:center") <- rm
#         }
#         if (scale) {
#             attr(x, "scaled:scale") <- csd
#         }
#     }
#     return(x)
# }
