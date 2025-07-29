#' Get coordinates of repelled labels from ggrepel
#'
#' Adapted from https://github.com/slowkow/ggrepel/issues/24. Labels are
#' ploted very late in the plotting process, i.e. their positions are not
#' pre-computed or so. Hence, it is not straight forward to extract their
#' repelled positions.
#'
#' Based on https://github.com/slowkow/ggrepel/issues/250 and
#' https://github.com/slowkow/ggrepel/issues/24
#'
#' @param obj data frame with first columns being xy-coords and optionally
#' third column with labels and optionally fourth columns with groups
#' (see example)
#' @param width passed to grid::viewport, may be subject to iteration to obtain
#' optimal repelled coordinates
#' @param height passed to grid::viewport
#' @param repel_fun_args arguments to fun
#' @param fun repel function from ggrepel package: label or text
#'
#' @return data frame
#' @export
#'
#' @examples
#' library(ggplot2)
#' # 10 overlapping label positions
#' # first two columns are xy, third is optionally the label column
#' x <- rnorm(10)
#' y <- rnorm(10)
#' label <- letters[1:10]
#' df <- data.frame(x = c(x,x), y = c(y,y), z = c(label, label))
#' # repel on the usual way
#' ggplot(df, aes(x = x, y = y, label = z)) +
#'     geom_point() +
#'     ggrepel::geom_label_repel()
#'
#' # hard code the repel
#' df2 <- get_repel_coords(obj = df)
#' ggplot(df2, aes(x = x, y = y, label = z)) +
#'     geom_point(data = df) +
#'     geom_label()
#'
#' # repel coords with xy-columns only
#' get_repel_coords(df[,c(1,2)])
#'
#' # when overlapping label are not exactly at same positions but close
#' # provide a group column to indicate which labels are expected to overlap
#' # their positions are aligned by averaging
#' # this will produce a better repelling
#' # so, optionally the fourth column: a grouping column
#' df[1:10,1] <- df[1:10,1] + 0.5
#' df$group <- rep(1:10, 2)
#' get_repel_coords(obj = df)
#'
#' # when there is not label column but a group column
#' # create a dummy label column at third position
#' df3 <- df[,-3]
#' df3$dummylabel <- "."
#' df3 <- df3[,c(1,2,4,3)]
get_repel_coords <- function(obj,
                             width = 1,
                             height = 1,
                             repel_fun_args = list(max.overlaps = Inf,
                                                   max.time = 1,
                                                   max.iter = 2e4),
                             fun = ggrepel::geom_label_repel) {

    if (!requireNamespace("Gmisc", quietly = T)) {
        utils::install.packages("Gmisc")
    }

    grid::grid.newpage()
    grid::pushViewport(grid::viewport(width = width, height = height))

    fun <- match.fun(fun)

    xvar <- names(obj)[1]
    yvar <- names(obj)[2]
    lvar <- NULL
    if (ncol(obj) > 2) {
        lvar <- names(obj)[3] # label
    }

    if (ncol(obj) > 3) {
        # group column there
        obj <- dplyr::mutate(obj,
                             !!xvar := mean(!!rlang::sym(xvar)),
                             !!yvar := mean(!!rlang::sym(yvar)),
                             .by = !!rlang::sym(names(obj)[4]))
    }

    g <- ggplot2::ggplot(obj, ggplot2::aes(
        x = !!rlang::sym(xvar),
        y = !!rlang::sym(yvar))) +
        geom_point()


    if (ncol(obj) > 2) {
        repel_fun_args <- c(list(mapping = ggplot2::aes(label = !!rlang::sym(lvar))), repel_fun_args)
    } else {
        repel_fun_args <- c(list(label = "."), repel_fun_args)
    }
    set.seed(42)
    g <- g + Gmisc::fastDoCall(what = fun, args = repel_fun_args)

    plotlims <- brathering::gg_lims(g)
    tree <- get_tree(gPath = "labelrepeltree", plot = g)
    if (is.null(tree)) {
        # geom_text_repel
        tree <- get_tree(gPath = "textrepeltree", plot = g)
    }
    children <- grep(pattern = "textrepelgrob", x = grid::childNames(tree), value = T)

    if (ncol(obj) > 2) {
        names(children) <- obj[,3]
    }
    res <- purrr::map_dfr(
        children,
        get_xy,
        tree = tree,
        plotlims = plotlims,
        xvar = xvar,
        yvar = yvar,
        .id = lvar
    )
    res <- cbind(res, obj[,which(!colnames(obj) %in% colnames(res)), drop = F])
    res <- res[,names(obj)]
    return(res)
}

get_tree <- function(gPath, plot) {
    grid::getGrob(
      gTree = grid::grid.force(x = ggplot2::ggplotGrob(plot), draw = F),
      gPath = gPath,
      grep = T
    )
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
