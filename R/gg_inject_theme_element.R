#' Alter a specific ggplot2 theme element
#'
#' Overwriting other parts of elem except elem_sub is avoided.
#'
#' @param theme_args list of ggplot theme elements
#' @param elem element name
#' @param elem_sub element subtype
#' @param value value to write
#'
#' @returns theme_args list with new value
#' @export
#'
#' @examples
#' theme_args <- ggplot2::theme_get()
#' theme_args <- gg_inject_theme_element(theme_args, "axis.text.x", "face", value = "italic")
gg_inject_theme_element <- function(theme_args,
                                    elem,
                                    elem_sub,
                                    value) {
    if (is.null(theme_args[[elem]])) {
        theme_args[[elem]] <- ggplot2::theme_get()[[elem]]
    }
    theme_args[[elem]][[elem_sub]] <- value
    return(theme_args)
}
