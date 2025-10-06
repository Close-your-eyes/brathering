#' Get axis breaks in order from ggplot
#'
#' @param ggobj ggplot object
#'
#' @returns list of x and y breaks
#' @export
#'
#' @examples
gg_get_axis_text <- function(ggobj) {

    ggobj <- ggplot2::ggplot_build(ggobj)
    xorder <- ggobj[["layout"]][["panel_scales_x"]][[1]][["range"]][["range"]]
    yorder <- ggobj[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]]

    return(list(x = xorder, y = yorder))
}
