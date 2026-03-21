#' Row wise signal-to-noise
#'
#' @param mat1 numeric matrix1
#' @param mat2 numeric matrix2
#' @param eps epsilon to avoid division by zero
#'
#' @returns numeric vector
#' @export
#'
#' @examples
s2n <- function(mat1, mat2, eps = 1e-6) {

    rn1 <- rownames(mat1)
    rn2 <- rownames(mat2)

    # If rownames exist, use them to align matrices
    if (!is.null(rn1) && !is.null(rn2)) {

        common <- intersect(rn1, rn2)

        if (length(common) == 0) {
            stop("No overlapping rownames between mat1 and mat2.")
        }

        if (nrow(mat1) != nrow(mat2) || !setequal(rn1, rn2)) {

            message(
                sprintf(
                    "Reducing matrices to %d intersecting rows (mat1: %d, mat2: %d).",
                    length(common), nrow(mat1), nrow(mat2)
                )
            )
        }

        mat1 <- mat1[common, , drop = FALSE]
        mat2 <- mat2[common, , drop = FALSE]
    } else {
        # fallback when rownames are missing
        if (nrow(mat1) != nrow(mat2)) {
            stop("Matrices must have equal number of rows when rownames are missing.")
        }
    }

    m1 <- Matrix::rowMeans(mat1)
    m2 <- Matrix::rowMeans(mat2)
    s1 <- rowsds(mat1)
    s2 <- rowsds(mat2)

    s2n <- (m1 - m2) / (s1 + s2 + eps)

    return(s2n)
}

#' Row wise standard deviation
#'
#' Works with sparse matrices
#'
#' @param x numeric matrix
#'
#' @returns numeric vector
#' @export
#'
#' @examples
rowsds <- function(x) {
    m  <- Matrix::rowMeans(x)
    m2 <- Matrix::rowMeans(x^2)
    sqrt(pmax(m2 - m^2, 0))
}
