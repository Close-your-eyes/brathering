#' Get axis limits of ggplot object
#'
#' @param ggobj ggplot2 object
#'
#' @return list of numeric vectors
#' @export
#'
#' @examples
#' ggobj <- ggplot2::ggplot(data = data.frame(xx = rnorm(100, 20,2),
#'                                            yy = rnorm(100,10,10)),
#'                          ggplot2::aes(xx,yy)) +
#'          ggplot2::geom_point()
#' gg_lims(ggobj)

gg_lims <- function(ggobj) {

    plot_build <- ggplot2::ggplot_build(ggobj)
    x_limits <- plot_build$layout$panel_params[[1]]$x.range
    y_limits <- plot_build$layout$panel_params[[1]]$y.range

    xvar <- "x"
    yvar <- "y"
    xmap <- ggobj[["mapping"]][["x"]]
    if (!is.null(xmap)) {
        xvar <- rlang::as_name(rlang::quo_get_expr(xmap))
    }
    ymap <- ggobj[["mapping"]][["y"]]
    if (!is.null(ymap)) {
        yvar <- rlang::as_name(rlang::quo_get_expr(ymap))
    }

    #x_data <- range(ggobj$data[[1]][,1])
    #y_data <- range(ggobj$data[[1]][,2])
    return(stats::setNames(list(
        x_limits,
        y_limits
        #x_data = x_data,
        #y_data = y_data
    ), c(xvar, yvar)))
}


