#' Get theme element from ggplot2 object
#'
#' @param ggobj ggplot2 object
#' @param element theme element name
#'
#' @returns
#' @export
#'
#' @examples
gg_get_theme_element <- function(ggobj,
                                 element = "plot.background") {
    ggplot2::calc_element(element, ggplot2::ggplot_build(ggobj)$plot$theme)
}
