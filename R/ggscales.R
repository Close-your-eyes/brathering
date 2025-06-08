#' Spectral color scale
#'
#' Shortcut function to a spectral color scale.
#'
#' @param colors see ggplot2::scale_color_gradient
#' @param values see ggplot2::scale_color_gradient
#' @param name see ggplot2::scale_color_gradient
#' @param na.value see ggplot2::scale_color_gradient
#' @param guide see ggplot2::scale_color_gradient
#' @param ... see ggplot2::scale_color_gradient
#'
#' @return ggplot2::scale_color_gradient
#' @export
#'
#' @examples
scale_color_spectral <- function(colors = colrr::col_pal("RColorBrewer::Spectral", direction = -1),
                                 values = NULL,
                                 name = waiver(),
                                 na.value = "grey50",
                                 guide = "colorbar",
                                 ...) {
    if (!requireNamespace("colrr", quietly = T)) {
        devtools::install_github("https://github.com/Close-your-eyes/colrr")
    }
    if (is.null(values)) {
        values <- seq(0, 1, length.out = length(colors))
    }
    ggplot2::scale_color_gradient(
        colours = colors,
        values = scales::rescale(values),
        name = name,
        na.value = na.value,
        guide = guide,
        ...
    )
}

#' Spectral fill scale
#'
#' Shortcut function to a spectral fill scale.
#'
#' @param colors see ggplot2::scale_fill_gradient
#' @param values see ggplot2::scale_fill_gradient
#' @param name see ggplot2::scale_fill_gradient
#' @param na.value see ggplot2::scale_fill_gradient
#' @param guide see ggplot2::scale_fill_gradient
#' @param ... see ggplot2::scale_fill_gradient
#'
#' @return ggplot2::scale_fill_gradient
#' @export
#'
#' @examples
scale_fill_spectral <- function(colors = colrr::col_pal("RColorBrewer::Spectral", direction = -1),
                                values = NULL,
                                name = waiver(),
                                na.value = "grey50",
                                guide = "colorbar",
                                ...) {
    if (!requireNamespace("colrr", quietly = T)) {
        devtools::install_github("https://github.com/Close-your-eyes/colrr")
    }
    if (is.null(values)) {
        values <- seq(0, 1, length.out = length(colors))
    }
    ggplot2::scale_fill_gradient(
        colours = colors,
        values = scales::rescale(values),
        name = name,
        na.value = na.value,
        guide = guide,
        ...
    )
}

#' Custom color scale
#'
#' @param colors see ggplot2::scale_color_manual
#' @param name see ggplot2::scale_color_manual
#' @param na.value see ggplot2::scale_color_manual
#' @param ... see ggplot2::scale_color_manual
#'
#' @return ggplot2::scale_color_manual
#' @export
#'
#' @examples
scale_color_custom <- function(colors = colrr::col_pal("custom"),
                               name = waiver(),
                               na.value = "grey50",
                               ...) {
    if (!requireNamespace("colrr", quietly = T)) {
        devtools::install_github("https://github.com/Close-your-eyes/colrr")
    }
    ggplot2::scale_color_manual(
        values = colors,
        name = name,
        na.value = na.value,
        ...
    )
}

#' Custom fill scale
#'
#' @param colors see ggplot2::scale_fill_manual
#' @param name see ggplot2::scale_fill_manual
#' @param na.value see ggplot2::scale_fill_manual
#' @param ... see ggplot2::scale_fill_manual
#'
#' @return ggplot2::scale_fill_manual
#' @export
#'
#' @examples
scale_fill_custom <- function(colors = colrr::col_pal("custom"),
                              name = waiver(),
                              na.value = "grey50",
                              ...) {
    if (!requireNamespace("colrr", quietly = T)) {
        devtools::install_github("https://github.com/Close-your-eyes/colrr")
    }
    ggplot2::scale_fill_manual(
        values = colors,
        name = name,
        na.value = na.value,
        ...
    )
}
