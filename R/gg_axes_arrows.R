#' Add axes arrows to ggplot
#'
#' @param ggobj ggplot object
#' @param rel_len relative length of axis
#' @param add_names add axis titles?
#' @param arrow_args arguments to grid::arrow
#' @param segment_args arguments to ggplot2::geom_segment
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
                           rel_len = 0.1,
                           add_names = T,
                           annotate_args = list(size = 3,
                                                color = "black"),
                           arrow_args = list(angle = 25,
                                             length = grid::unit(0.2, "cm"),
                                             type = "open"),
                           segment_args = list(linewidth = 0.2,
                                               color = "black")) {

    brathering::gg_lims(ggobj)

    lims <-  brathering::gg_lims(ggobj)
    xlim <- lims[[1]]
    ylim <- lims[[2]]

    xlen <- diff(xlim) * rel_len
    ylen <- diff(ylim) * rel_len

    x0 <- xlim[1]
    y0 <- ylim[1]

    # ggobj$theme$plot.margin
    # enforce_min_margin(ggobj)
    # plot.margin = grid::unit(c(1,1,10,10), "pt")
    ggobj <- ggobj +
        ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                       axis.line.y = ggplot2::element_blank()) +
        do.call(ggplot2::geom_segment, args = c(list(x = x0, xend = x0 + xlen,
                                                     y = y0, yend = y0,
                                                     inherit.aes = FALSE,
                                                     arrow = do.call(grid::arrow, args = arrow_args)),
                                                segment_args)) +
        do.call(ggplot2::geom_segment, args = c(list(x = x0, xend = x0,
                                                     y = y0, yend = y0 + ylen,
                                                     inherit.aes = FALSE,
                                                     arrow = do.call(grid::arrow, args = arrow_args)),
                                                segment_args)) +
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = F, clip = "off")

    if (add_names) {
        ggobj <- ggobj +
            do.call(ggplot2::annotate, args = c(list(geom = "text", x = x0 + xlen*1.1, y = y0, label = names(lims)[1], hjust = 0),
                                                annotate_args)) +
            do.call(ggplot2::annotate, args = c(list(geom = "text", x = x0, y = y0 + ylen*1.1, label = names(lims)[2], hjust = 0, angle = 90),
                                                annotate_args))
    }

    return(ggobj)
}

enforce_min_margin <- function(p, min_pt = 10) {
    # Merge theme to get actual margins
    th <- ggplot2::theme_get() %+replace% p$theme

    m <- th$plot.margin
    if (is.null(m)) {
        stop("plot.margin is NULL (no margin defined).")
    }

    # Extract original unit
    original_unit <- attr(m, "unit")

    # Helper to convert numeric value in original unit -> points
    to_pt <- function(x) {
        as.numeric(grid::convertUnit(unit(x, original_unit), "pt"))
    }

    # Helper to convert points -> original unit
    pt_to_original <- function(pt_val) {
        as.numeric(grid::convertUnit(unit(pt_val, "pt"), original_unit))
    }

    # Convert all four sides to points
    sides_pt <- c(
        top    = to_pt(m[[1]]),
        right  = to_pt(m[[2]]),
        bottom = to_pt(m[[3]]),
        left   = to_pt(m[[4]])
    )

    # Apply rule: if bottom/left < min_pt → enforce minimum
    new_bottom <- if (sides_pt["bottom"] < min_pt) pt_to_original(min_pt) else m[[3]]
    new_left   <- if (sides_pt["left"]   < min_pt) pt_to_original(min_pt) else m[[4]]

    # Rebuild margin in original units
    # margin(
    #     t = m[[1]],
    #     r = m[[2]],
    #     b = new_bottom,
    #     l = new_left,
    #     unit = original_unit
    # )
    return(c(m[[1]], m[[2]], new_bottom, new_left))
}
