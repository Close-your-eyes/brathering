#' Get coordinates of repelled labels from ggrepel
#'
#' Adapted from https://github.com/slowkow/ggrepel/issues/24. Labels are
#' ploted very late in the plotting process, i.e. their positions are not
#' pre-computed or so. Hence, it is not straight forward to extract their
#' repelled positions.
#'
#' @param obj ggplot object or data frame with first columns being xy-coords
#' @param width passed to grid::viewport, may be subject to iteration to obtain
#' optimal repelled coordinates
#' @param height passed to grid::viewport
#' @param ... arguments to ggrepel::geom_text_repel
#'
#' @return data frame
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x=rnorm(10), y=rnorm(10), z = letters[1:10])
#' gg <- ggplot(df, aes(x,y)) +
#'     geom_point()
#' gg <-
#'     gg +
#'     ggrepel::geom_label_repel(aes(label = z)) +
#'     geom_text(data = dplyr::left_join(df, get_repel_coords(gg)),
#'               aes(x = x_repel,y = y_repel, label = z), color = "hotpink")
#' gg
get_repel_coords <- function(obj, width = 2, height = 2, ...) {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(width = width, height = height))

    if (is.data.frame(obj)) {
        obj <- ggplot2::ggplot(obj,
                                 ggplot2::aes(
                                     !!rlang::sym(names(obj)[1]),
                                     !!rlang::sym(names(obj)[2]))) +
            ggplot2::geom_point()
    }

    xvar <- rlang::as_name(rlang::quo_get_expr(obj[["mapping"]][["x"]]))
    yvar <- rlang::as_name(rlang::quo_get_expr(obj[["mapping"]][["y"]]))

    g <- obj +
        ggrepel::geom_text_repel(
            mapping = ggplot2::aes(!!rlang::sym(xvar), !!rlang::sym(yvar)),
            label = ".",
            data = obj[["data"]],
            max.overlaps = Inf,
            ...)

    labelvar <- obj[["labels"]][["label"]]
    plotlims <- brathering::gg_lims(obj)
    tree <- grid::getGrob(gTree = grid::grid.force(x = ggplot2::ggplotGrob(g), draw = F), gPath = "textrepeltree", grep = T)
    children <- grep(pattern = "textrepelgrob", x = grid::childNames(tree), value = T)

    get_xy <- function(n, tree, plotlims) {
        grob <- grid::getGrob(tree, n)
        data.frame(
            x_repel = plotlims[["x"]][1] + diff(plotlims[["x"]]) * grid::convertX(grob$x, "native", valueOnly = T),
            y_repel = plotlims[["y"]][1] + diff(plotlims[["y"]]) * grid::convertY(grob$y, "native", valueOnly = T))
    }

    return(cbind(obj[["data"]][,c(xvar, yvar, labelvar)],
                 purrr::map_dfr(children, get_xy, tree = tree, plotlims = plotlims)))
}

