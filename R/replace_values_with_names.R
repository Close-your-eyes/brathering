#' Make function arguments their value
#'
#' @param code_block
#'
#' @returns character to prep with cat()
#' @export
#'
#' @examples
#' x <- replace_values_with_names(' order = T,
#'                            fill = colrr::col_pal("custom"),
#'                            color = "white",
#'                            radius_inside = 0.3,
#'                            label_outside = c("none", "abs", "rel"),
#'                            label_inside = c("rel", "abs", "none"),
#'                            label_rel_cutoff = 0,
#'                            label_size = 5,
#'                            label_radius_inside = 0.75,
#'                            label_radius_outside = 1.1,
#'                            label_angle_inside = NULL, # circle or numeric
#'                            label_angle_outside = NULL, # circle or numeric
#'                            label_overlap = c("ignore", "alternate", "outside"),
#'                            overlap_outside_radius = 1.1,
#'                            label_rel_pct = F,
#'                            label_rel_dec = 2,
#'                            legend_title = NULL,
#'                            theme_args = list(panel.grid = ggplot2::element_blank(),
#'                                              axis.title = ggplot2::element_blank(),
#'                                              axis.text = ggplot2::element_blank(),
#'                                              axis.ticks = ggplot2::element_blank())')
#' cat(x)
replace_values_with_names <- function(code_block) {
    # Split into lines
    lines <- unlist(strsplit(code_block, "\n"))

    # Process each line
    processed <- sapply(lines, function(line) {
        # Remove comments first
        line_no_comment <- sub("#.*$", "", line)

        # Extract argument name
        if (grepl("=", line_no_comment)) {
            arg_name <- trimws(strsplit(line_no_comment, "=")[[1]][1])
            paste0(arg_name, " = ", arg_name, ",")
        } else {
            line_no_comment # Leave unchanged if no "="
        }
    })

    # Collapse back into a string
    paste(processed, collapse = "\n")
}


