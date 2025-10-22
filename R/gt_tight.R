#' Make a tight gt table by default
#'
#' @param df data frame
#' @param font_size font size
#' @param font_family family
#'
#' @returns gt table
#' @export
#'
#' @examples
gt_tight <- function(df,
                     font_size = 11,
                     font_family = "Arial") {
    gt::gt(df) |>
        gt::tab_options(
            table.font.size = gt::px(font_size),
            table.font.names = font_family,
            data_row.padding = gt::px(0),
            column_labels.padding = gt::px(0),
            table.border.top.width = gt::px(0),
            #table.border.bottom.width = gt::px(0),
            #heading.border.bottom.width = gt::px(0),
            #row_group.border.top.width = gt::px(0),
            #row_group.border.bottom.width = gt::px(0),
            column_labels.border.top.width = gt::px(0),
            #column_labels.border.bottom.width = gt::px(0),
            #table_body.border.top.width = gt::px(0),
            table_body.border.bottom.width = gt::px(0)
        )

    # gt::opt_css(
    #   css = "
    #     .gt_table {
    #       border-collapse: collapse !important;
    #       margin: 0 !important;
    #     }
    #     .gt_row, .gt_col_headings, .gt_heading, .gt_footnote {
    #       padding: 0px !important;
    #     }
    #     .gt_col_heading {
    #       font-weight: 600 !important;
    #     }
    #     .gt_row {
    #       border-bottom: none !important;
    #     }
    #     ")

}
