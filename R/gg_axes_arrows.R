#' Add axes arrows to ggplot
#'
#' @param ggobj ggplot object
#' @param rel_len relative length of axis
#' @param add_names add axis titles?
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(20, 2, 1),
#' y = rnorm(20, 3, 1))
#' p <- ggplot(df, aes(x, y)) +
#'     geom_point()
#' gg_axes_arrows(p)
gg_axes_arrows <- function(ggobj,
                           rel_len = 0.3,
                           add_names = T) {

    gb <- ggplot2::ggplot_build(ggobj)
    xlim <- gb$layout$panel_params[[1]]$x.range
    ylim <- gb$layout$panel_params[[1]]$y.range

    xlen <- diff(xlim) * rel_len
    ylen <- diff(ylim) * rel_len

    x0 <- xlim[1]
    y0 <- ylim[1]

    ggobj <- ggobj +
        ggplot2::geom_segment(x = x0, xend = x0 + xlen,
                              y = y0, yend = y0,
                              inherit.aes = FALSE,
                              arrow = grid::arrow(length = grid::unit(0.3, "cm"), type = "closed")) +
        ggplot2::geom_segment(x = x0, xend = x0,
                              y = y0, yend = y0 + ylen,
                              inherit.aes = FALSE,
                              arrow = grid::arrow(length = grid::unit(0.3, "cm"), type = "closed")) +
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off")

    if (add_names) {
        ggobj <- ggobj +
            ggplot2::annotate(geom = "text", x = x0 + xlen*1.1, y = y0, label = ggobj$labels$x, hjust = 0) +
            ggplot2::annotate(geom = "text", x = x0, y = y0 + ylen*1.1, label = ggobj$labels$y, hjust = 0, angle = 90)
    }

    return(ggobj)
}
