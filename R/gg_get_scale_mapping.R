#' Get mapped colours or fills from a ggplot2 object
#'
#' @param ggobj A ggplot2 plot object.
#' @param aesthetic The discrete aesthetic to retrieve. Use \code{"colour"}
#'   (the default), \code{"color"}, or \code{"fill"}.
#'
#' @returns A named vector of mapped colours or fills in scale order.
#'   The names are the category values from the trained scale. The function
#'   raises an error if the requested scale is missing or is continuous.
#' @seealso \code{\link{gg_get_theme_element}()} for theme colours and fills.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     p <- ggplot2::ggplot(
#'         mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl), fill = factor(cyl))
#'     ) +
#'         ggplot2::geom_point(shape = 21) +
#'         ggplot2::scale_fill_manual(
#'             values = c("4" = "white", "6" = "grey70", "8" = "grey30")
#'         )
#'
#'     gg_get_scale_mapping(p)
#'     gg_get_scale_mapping(p, "fill")
#' }
gg_get_scale_mapping <- function(ggobj, aesthetic = "colour") {
    aesthetic <- match.arg(aesthetic, c("colour", "color", "fill"))
    if (aesthetic == "color") {
        aesthetic <- "colour"
    }
    brathering:::.ensure_packages(c("ggplot2"))

    built <- ggplot2::ggplot_build(ggobj)
    scale <- built$plot$scales$get_scales(aesthetic)
    if (is.null(scale)) {
        stop(sprintf("The plot has no '%s' scale.", aesthetic), call. = FALSE)
    }
    if (!scale$is_discrete()) {
        stop(sprintf("The '%s' scale must be discrete.", aesthetic), call. = FALSE)
    }

    categories <- scale$get_limits()
    stats::setNames(scale$map(categories), categories)
}
