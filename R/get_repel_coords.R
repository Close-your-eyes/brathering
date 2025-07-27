#' Get coordinates of repelled labels from ggrepel
#'
#' Adapted from https://github.com/slowkow/ggrepel/issues/24. Labels are
#' ploted very late in the plotting process, i.e. their positions are not
#' pre-computed or so. Hence, it is not straight forward to extract their
#' repelled positions.
#'
#' @param obj data frame with first columns being xy-coords and third column
#' labels
#' @param width passed to grid::viewport, may be subject to iteration to obtain
#' optimal repelled coordinates
#' @param height passed to grid::viewport
#' @param ... arguments to ggrepel::geom_text_repel
#' @param max.overlaps ggrepel::geom_text_repel argument
#'
#' @return data frame
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x=rnorm(10), y=rnorm(10), z = letters[1:10])
#' gg <-
#'     ggplot(df, aes(x = x, y = y, label = z)) +
#'     geom_point() +
#'     ggrepel::geom_label_repel() +
#'     geom_text(data = get_repel_coords(obj = df), color = "hotpink")
#' gg
get_repel_coords <- function(obj,
                             width = 1,
                             height = 1,
                             max.overlaps = Inf,
                             ...) {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(width = width, height = height))

    g <- ggplot2::ggplot(obj,
                         ggplot2::aes(
                             x = !!rlang::sym(names(obj)[1]),
                             y = !!rlang::sym(names(obj)[2]))) +
        ggrepel::geom_text_repel(
            mapping = ggplot2::aes(label = !!rlang::sym(names(obj)[3])),
            max.overlaps = max.overlaps,
            ...)

    plotlims <- brathering::gg_lims(g)
    tree <- grid::getGrob(gTree = grid::grid.force(x = ggplot2::ggplotGrob(g), draw = F), gPath = "textrepeltree", grep = T)
    children <- grep(pattern = "textrepelgrob", x = grid::childNames(tree), value = T)
    names(children) <- obj[,3]

    return(purrr::map_dfr(
                   children,
                   get_xy,
                   tree = tree,
                   plotlims = plotlims,
                   xvar = names(obj)[1],
                   yvar = names(obj)[2],
                   .id = names(obj)[3]
                 ))
}

get_xy <- function(n,
                   tree,
                   plotlims,
                   xvar,
                   yvar) {
    grob <- grid::getGrob(tree, n)
    df <- data.frame(
        x = plotlims[[1]][1] + diff(plotlims[[1]]) * grid::convertX(grob$x, "native", valueOnly = T),
        y = plotlims[[2]][1] + diff(plotlims[[2]]) * grid::convertY(grob$y, "native", valueOnly = T))
    names(df) <- c(xvar, yvar)
    return(df)
}
