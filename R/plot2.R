#' Plot data quickly with scattermore
#'
#' A convenient wrapper around scattermore::scattermoreplot to have easy
#' color scale and legend.
#'
#' @param x matrix or data frame, col1 becomes x, col2 becomes y
#' @param color name of column for color or .density_r or .density_s for
#' coloring according to point density raw or scaled between 0 and 1
#' @param palette color palette
#' @param colortype type of color scale
#' @param legend where to plot legend, NULL for no legend
#' @param ... arguments to scattermore::scattermoreplot
#' @param transform transform data before plotting, provide function as character,
#' e.g. "log" or "log10" or "sqrt"
#' @param least_freq_col_top plot the color groups in ascending order of
#' frequency to avoid burying low frequent groups
#' @param size dot size
#' @param color_text_pos NULL or c("tl", "tr", "bl", "br"); if NULL not plotted
#' @param color_text which string to plot, if NULL color is chosen
#'
#' @return nothing, scattermore plot is plotted
#' @export
#'
#' @examples
#' brathering::plot2(brathering::points_2d_circ(100))
plot2 <- function(x,
                  color = NULL,
                  size = 4,
                  palette = NULL,
                  colortype = c(
                    "d",
                    "c",
                    "discrete",
                    "continuous"
                  ),
                  legend = c(
                    "bottomright",
                    "bottom",
                    "bottomleft",
                    "left",
                    "topleft",
                    "top",
                    "topright",
                    "right",
                    "center"
                  ),
                  transform = NULL,
                  least_freq_col_top = TRUE,
                  color_text_pos = "tl",
                  color_text = NULL,
                  discrete_lvls = 8,
                  ...) {

    if (!requireNamespace("colrr", quietly = T)) {
        devtools::install_github("Close-your-eyes/colrr")
    }

    if (missing(x) || is.null(x)) {
        message("plot2: x missing.")
        return(NULL)
    }

    if (!is.data.frame(x) && !is.matrix(x)) {
        x <- data.frame(index = seq_along(x), y = x)
    }

    colortype <- rlang::arg_match(colortype)
    col <- grDevices::rgb(0, 0, 0, 1)

    if (!is.null(legend) && !is.logical(legend)) {
        legend <- rlang::arg_match(legend)
    }

    xaxt <- NULL
    yaxt <- NULL

    # if (tibble::is_tibble(x)) {
    #     x <- as.data.frame(x)
    # }
    x <- as.data.frame(x)

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

            col <- as.character(palette[scales::rescale(x[,color], to = c(1, 100))])
        }

        if (colortype %in% c("d", "discrete")) {

            if (is.numeric(x[,color])) {
                # make numeric to discrete levels
                x[,color] <- cut(x[,color], discrete_lvls)
                if (is.null(palette)) {
                    palette <- colrr::col_pal("Spectral", n = 100, direction = -1)
                    palette <- grDevices::colorRampPalette(palette)(discrete_lvls)
                }
            }
            if (!is.factor(x[,color])) {
                col_fac <- as.factor(x[,color])
                col_lvl <- sort(levels(col_fac))
            } else {
                col_fac <- x[,color]
                col_lvl <- levels(col_fac)
            }

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
        x[["color"]] <- col

        if (least_freq_col_top && length(unique(col)) > 1) {
            # issue https://github.com/exaexa/scattermore/issues/25
            # dirty solution: replicate
            colfreqs <- table(x[["color"]])
            colfreqs <- sort(colfreqs, decreasing = T)
            x <- split(x, x[["color"]])
            for (i in 2:length(x)) {
                x[[i]] <- dplyr::bind_rows(replicate(20, x[[i]], simplify = FALSE))
            }
            x <- dplyr::bind_rows(x[names(colfreqs)])
        }
    }

    if (!is.null(transform)) {
        trans <- match.fun(transform)
        for (i in c(1,2)) {
            if (is.numeric(x[,i])) {
                x[,i] <- trans(x[,i])
            }
        }
    }

    filterNAcols <- c(colnames(x)[1],colnames(x)[2],"color")
    filterNAcols <- filterNAcols[which(filterNAcols %in% colnames(x))]
    x <- tidyr::drop_na(x[,filterNAcols])
    if ("color" %in% colnames(x)) {
        col <- x[["color"]]
    }
    if (nrow(x) == 0) {
        message("all lines of x contained NA.")
        return(NULL)
    }

    scattermore::scattermoreplot(
        x[,1],
        x[,2],
        cex = size,
        col = col,
        xaxt = xaxt,
        yaxt = yaxt,
        xlab = colnames(x)[1],
        ylab = colnames(x)[2],
        ...
    )

    if (!is.null(color_text_pos) && !is.null(color)) {
        if (is.null(color_text)) {
            color_text <- color
        }
        add_color_text(txt = color_text, pos = color_text_pos)
    }


    if (!is.null(color) && colortype %in% c("d", "discrete") && !is.null(legend) && !is.logical(legend)) {
        legend(x = legend, legend = names(col_vec), fill = col_vec, cex = 0.8)
    }

    if (!is.null(xaxt)) {
        graphics::axis(1, at = lvlsnumx, labels = lvlsx)
    }
    if (!is.null(yaxt)) {
        graphics::axis(2, at = lvlsnumy, labels = lvlsy)
    }
}

add_color_text <- function(txt, pos = c("tl", "tr", "bl", "br")) {
    pos <- rlang::arg_match(pos)
    u <- par("usr")  # (x1, x2, y1, y2)

    if (pos == "tl") {
        # Top-left
        text(u[1]*0.98, u[4]*0.98, txt, adj=c(0,1))
    } else if (pos == "tr") {
        # Top-right
        text(u[2]*0.98, u[4]*0.98, txt, adj=c(1,1))
    } else if (pos == "bl") {
        # Bottom-left
        text(u[1]*0.98, u[3]*0.98, txt, adj=c(0,0))
    } else if (pos == "br") {
        # Bottom-right
        text(u[2]*0.98, u[3]*0.98, txt, adj=c(1,0))
    }
}
