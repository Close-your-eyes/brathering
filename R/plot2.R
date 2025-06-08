#' Plot data quickly with scattermore
#'
#' A convenient wrapper to have easy color scale and legend.
#'
#' @param x matrix or data frame, col1 becomes x, col2 becomes y
#' @param color name of column for color or .density_r or .density_s for
#' coloring according to point density
#' @param palette color palette
#' @param colortype type of color scale
#' @param legend plot legend?
#' @param ... arguments to scattermore::scattermoreplot
#'
#' @return nothing, scattermore plot is plotted
#' @export
#'
#' @examples
#' brathering::plot2(brathering::points_2d_circ(100))
plot2 <- function(x,
                  color = NULL,
                  palette = NULL,
                  colortype = c("d", "c", "discrete", "continuous"),
                  legend = T,
                  ...) {

    colortype <- rlang::arg_match(colortype)
    col <- grDevices::rgb(0, 0, 0, 1)

    xaxt <- NULL
    yaxt <- NULL

    if (is.data.frame(x)) {
        if (!is.numeric(x[,1])) {
            # a complicated, but to get a vector for xlab
            lvlsx <- levels(as.factor(x[,1]))
            lvlsnumx <- as.numeric(as.factor(lvlsx))
            lvlsconvx <- stats::setNames(lvlsnumx, lvlsx)
            xaxt <- "n"
            x[,1] <- unname(lvlsconvx[x[,1]])
        }
        if (!is.numeric(x[,2])) {
            #x[,2] <- as.numeric(as.factor(x[,2]))
            lvlsy <- levels(as.factor(x[,2]))
            lvlsnumy <- as.numeric(as.factor(lvlsy))
            lvlsconvy <- stats::setNames(lvlsnumy, lvlsy)
            yaxt <- "n"
            x[,2] <- unname(lvlsconvy[x[,2]])
        }
    }

    if (!is.null(color)) {
        if (!color %in% c(".density_r", ".density_s") && !color %in% colnames(x)) {
            message("color not found in colnames of x.")
            color <- NULL
        } else if (color %in% c(".density_r", ".density_s")) {
            x <- cbind(x, matrix(density_est(x, type = "2D")[[ifelse(color == ".density_r", "raw", "scaled")]], ncol = 1, dimnames = list(NULL, ".density")))
            colortype <- "c"
            color <- ".density"
        }

        if (colortype %in% c("continuous", "c")) {
            if (is.null(palette)) {
                palette <- colrr::col_pal("Spectral", n = 100, direction = -1)
            } else {
                if (length(palette) < 100) {
                    palette <- grDevices::colorRampPalette(palette)(100)
                } else if (length(palette) > 100) {
                    palette <- palette[round(seq(1, length(palette), length.out = 100))]
                }
            }
            col <- palette[1+99*x[,color]]
        }

        if (colortype %in% c("d", "discrete")) {
            col_fac <- as.factor(x[,color])
            col_lvl <- sort(levels(col_fac))
            col_num <- as.numeric(col_fac)
            if (is.null(palette)) {
                palette <- colrr::col_pal("custom", n = nlevels(col_fac))
            }
            if (length(palette) < length(unique(col_num))) {
                message("palette has insufficient values.")
            } else {
                col_vec <- stats::setNames(as.character(palette), col_lvl)
                col <- unname(col_vec[col_num])
            }
        }
    }

    scattermore::scattermoreplot(x[,1], x[,2], col = col, xaxt = xaxt, yaxt = yaxt, ...)

    if (!is.null(color) && colortype %in% c("d", "discrete") && legend) {
        legend("topleft", legend = names(col_vec), fill = col_vec, cex = 0.8)
    }

    if (!is.null(xaxt)) {
        axis(1, at = lvlsnumx, labels = lvlsx)
    }
    if (!is.null(yaxt)) {
        axis(2, at = lvlsnumy, labels = lvlsy)
    }
}
