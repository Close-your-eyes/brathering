#' Create a 3D plot
#'
#' @param object matrix or data frame
#' @param x name of x column, if missing first column of object used
#' @param y name of y column
#' @param z name of z column
#' @param color name of color column
#' @param palette color palette
#' @param colortype discrete or continuous color scale
#' @param legend plot legend?
#' @param size size of points
#' @param type scatter or surface plot, only for plotly backend
#' @param backend plotly or rgl plot
#' @param transform transform data before plotting, provide function as character,
#' e.g. "log" or "log10" or "sqrt"
#' @param ... arguments to plotly::plot_ly or rgl::plot3d
#'
#' @return nothing but a plot plotted
#' @export
#'
#' @examples
#' df <- data.frame(
#' x = rnorm(1000),
#' y = rnorm(1000),
#' z = rnorm(1000),
#' col1 = sample(letters[1:5], 1000, replace = TRUE),
#' col2 = sample(c(1:10), 1000, replace = TRUE))
#' plot3d(df, color = ".density_s", colortype = "c", backend = "plotly")
#' plot3d(df, color = ".density_s", colortype = "c", backend = "rgl")
#' plot3d(df, color = "col1", colortype = "d", backend = "rgl")
#' rspheres <- replicate(5, hdos:::random_sphere(), simplify = FALSE)
#' rtorus <- replicate(5, hdos:::random_torus(), simplify = FALSE)
#' rcuboid <- replicate(5, hdos:::random_cuboid(), simplify = FALSE)
#' landscape <- hdos::bind_objects(c(rspheres, rtorus, rcuboid),
#'                                 missing_dim_fill = "runif")
#' landscape <- hdos::add_noise_points(landscape, density = 0.005)
#'
#' plot3d(dplyr::bind_cols(landscape), color = "name", legend = FALSE)
#' plot3d(dplyr::bind_cols(landscape), x = "x2", y = "x3", z = "x4",
#'        color = "name", legend = FALSE)
#' plot3d(dplyr::bind_cols(landscape), x = "x3", y = "x4", z = "x5",
#'        color = "name", legend = FALSE)
#' plot3d(dplyr::bind_cols(landscape), x = "x4", y = "x5", z = "x6",
#'        color = "name", legend = FALSE)
plot3d <- function(object,
                   x,
                   y,
                   z,
                   color = NULL,
                   palette = NULL,
                   colortype = c("d", "c", "discrete", "continuous"),
                   legend = T,
                   size = 2,
                   transform = NULL,
                   type = c("scatter3d", "mesh3d"),
                   backend = c("plotly", "rgl"),
                   ...) {
    if (!requireNamespace("colrr", quietly = T)) {
        devtools::install_github("Close-your-eyes/colrr")
    }
    colortype <- rlang::arg_match(colortype)
    type <- rlang::arg_match(type)
    backend <- rlang::arg_match(backend)

    if (missing(x)) {
        x <- colnames(object)[1]
    }
    if (missing(y)) {
        y <- colnames(object)[2]
    }
    if (missing(z)) {
        z <- colnames(object)[3]
    }

    object <- as.data.frame(object)


    if (!is.null(transform)) {
        trans <- match.fun(transform)
        for (i in c(x,y,z)) {
            if (is.numeric(object[,i])) {
                object[,i] <- trans(object[,i])
            }
        }
    }

    filterNAcols <- c(x,y,z,color)
    filterNAcols <- filterNAcols[which(filterNAcols %in% colnames(object))]
    object <- tidyr::drop_na(object[,filterNAcols])

    if (!is.null(color)) {
        if (!color %in% c(".density_r", ".density_s") && !color %in% colnames(object)) {
            message("color not found in colnames of x.")
            color <- NULL
        } else if (color %in% c(".density_r", ".density_s")) {
            object <- cbind(object, data.frame(.density = density_est(object, type = "3D")[[ifelse(color == ".density_r", "raw", "scaled")]]))
            colortype <- "c"
            color <- ".density"
        }
    }
    if (backend == "plotly") {
        if (is.null(color)) {
            colors <- NULL
            color_arg <- NULL
            marker <- NULL
        } else {
            if (!is.null(color) && colortype %in% c("d", "discrete")) {
                if (is.null(palette)) {
                    palette <- colrr::col_pal("custom", n = length(unique(object[,color])), direction = -1)
                }
                object[,color] <- as.factor(sort(object[,color]))
                colors <- rev(as.character(palette)) # reverse to match ggplot
                color_arg <- object[,color]
                marker <- NULL
            } else if (!is.null(color) && colortype %in% c("c", "continuous")) {
                if (is.null(palette)) {
                    palette <- colrr::col_pal("Spectral", direction = -1)
                }
                colors <- NULL
                color_arg <- NULL
                object[,color] <- as.numeric(object[,color])
                marker <- plotly_marker(data = object,
                                        color = color,
                                        size = size,
                                        colorscale = plotly_colorscale(palette),
                                        showscale = legend)

            }
        }

        p <- plotly::plot_ly(data = object,
                             x = object[,x],
                             y = object[,y],
                             z = object[,z],
                             color = color_arg,
                             type = type,
                             mode = "markers",
                             size = size,
                             colors = colors,
                             marker = marker,
                             showlegend = legend,
                             ...)
        p <- plotly::layout(p,
                            scene = list(
                                xaxis = list(title = x, range = rev(range(object[,x], na.rm = T))),
                                yaxis = list(title = y, range = rev(range(object[,y], na.rm = T))),
                                zaxis = list(title = z)
                            ))
        return(p)
    } else if (backend == "rgl") {

        if (is.null(color)) {
            palette <- rep("black", nrow(object))
        } else {
            if (!is.null(color) && colortype %in% c("d", "discrete")) {
                if (is.null(palette)) {
                    palette <- colrr::col_pal("custom", n = length(unique(object[,color])), direction = 1)
                }
                if (is.factor(object[,color])) {
                    inds <- as.numeric(factor(object[,color]))
                    palette <- palette[inds]
                } else if (is.character(object[,color])) {
                    object[,color] <- factor(object[,color], levels = unique(object[,color])) #sort?
                    inds <- as.numeric(object[,color])
                    palette <- palette[inds]
                }
            } else if (!is.null(color) && colortype %in% c("c", "continuous")) {
                if (is.null(palette)) {
                    palette <- colrr::col_pal("Spectral", n = 100, direction = -1)
                } else {
                    if (length(palette) < 100) {
                        palette <- grDevices::colorRampPalette(palette)(100)
                    } else if (length(palette) > 100) {
                        palette <- palette[round(seq(1, length(palette), length.out = 100))]
                    }
                }
                normvals <- scales::rescale(object[,color])
                palette <- palette[normvals*99+1]
            }
        }

        rgl::open3d()
        rgl::plot3d(x = object[,x],
                    y = object[,y],
                    z = object[,z],
                    xlab = x,
                    ylab = y,
                    zlab = z,
                    size = size,
                    col = palette,
                    aspect = F,
                    ...)
        if (colortype %in% c("d", "discrete") && legend) {
            # add legend
            legend_vec <- stats::setNames(unique(palette), unique(object[,color]))[levels(object[,color])]
            rgl::legend3d(
                "topright",
                legend = names(legend_vec),
                pch = 16,
                col = legend_vec,
                cex = 1,
                inset = c(0.02)
            )
        }
        rgl::rglwidget()
    }
}

plotly_colorscale <- function(colors) {
    n <- length(colors)
    stops <- seq(0, 1, length.out = n) # Evenly spaced values from 0 to 1
    lapply(seq_along(colors), function(i) list(stops[i], colors[i]))
}

plotly_marker <- function(data, color, ...) {
    list(color = data[[color]],
         cmin = min(data[[color]]),
         cmax = max(data[[color]]),
         ...)

}
