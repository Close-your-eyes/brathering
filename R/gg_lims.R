#' Get axis limits of ggplot object
#'
#' @param ggobj ggplot2 object
#'
#' @return list of numeric vectors
#' @export
#'
#' @examples
#' ggobj <- ggplot2::ggplot(data = data.frame(x = rnorm(100, 20,2),
#'                                            y = rnorm(100,10,10)),
#'                          ggplot2::aes(x,y)) +
#'          ggplot2::geom_point()
#' gg_lims(ggobj)

gg_lims <- function(ggobj) {
    plot_build <- ggplot2::ggplot_build(ggobj)
    x_limits <- plot_build$layout$panel_params[[1]]$x.range
    y_limits <- plot_build$layout$panel_params[[1]]$y.range
    x_data <- range(plot_build$data[[1]][,1])
    y_data <- range(plot_build$data[[1]][,2])
    return(list(
      x = x_limits,
      y = y_limits,
      x_data = x_data,
      y_data = y_data
    ))
}


