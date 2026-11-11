#' Get theme element from ggplot2 object
#'
#' @param ggobj ggplot2 object
#' @param element A ggplot2 theme element name. For example, use
#'   \code{"plot.background"}, \code{"panel.background"}, \code{"legend.key"},
#'   or \code{"strip.background"} to inspect a rectangle's \code{colour}
#'   (border) and \code{fill} (interior). Use \code{"plot.title"},
#'   \code{"axis.text.x"}, or \code{"legend.text"} for text \code{colour},
#'   and \code{"panel.grid.major"} or \code{"axis.ticks"} for line
#'   \code{colour}. The default is \code{"plot.background"}.
#'
#' @returns The theme element with inherited properties resolved. Its
#'   properties depend on the element type and the plot's theme.
#' @seealso \code{\link[ggplot2]{theme}()} for more element names.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     p <- ggplot2::ggplot() +
#'         ggplot2::theme(
#'             plot.background = ggplot2::element_rect(colour = "grey40", fill = "white"),
#'             axis.text.x = ggplot2::element_text(colour = "navy")
#'         )
#'     background <- gg_get_theme_element(p, "plot.background")
#'     background$colour
#'     background$fill
#'     gg_get_theme_element(p, "axis.text.x")$colour
#' }
gg_get_theme_element <- function(ggobj,
                                 element = "plot.background") {
    brathering:::.ensure_packages(c("ggplot2"))
    ggplot2::calc_element(element, ggplot2::ggplot_build(ggobj)$plot$theme)
}
