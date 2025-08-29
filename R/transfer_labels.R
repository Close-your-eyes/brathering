#' Transfer best matching/corresponding labels between two label columns
#'
#' Uses compare_labels. Observations in df1 and df2 do not need to match
#' exactly. Filtering join is applied.
#'
#' @param df1 data frame 1
#' @param df2 data frame 2
#' @param labelcol1 name of column with labels in df1
#' @param labelcol2 name of column with labels in df2
#' @param joincol name(s) of column(s) to join df1 and df2 by; NULL for all
#' matching colnames in df1 and df2; passed to 'by' of dplyr::inner_join
#'
#' @returns list, df1 and df2 with new columns of corresponding labels;
#' compared is return from compare_labels
#' @export
#'
#' @examples
#' df1 <- data.frame(
#'     label1 = c("A","A","A","B","B","C","C","C","C"),
#'     id = letters[1:9])
#' df2 <- data.frame(
#'     label2 = rev(c("X","X","Y","Y","Z","X","Z","Z","Z")),
#'     id = letters[9:1])
#' transfer_labels(df1, df2, "label1", "label2", "id")
transfer_labels <- function(df1,
                            df2,
                            labelcol1,
                            labelcol2,
                            joincol = NULL) {

    if (identical(labelcol1, labelcol2)) {
        stop("labelcol1  == labelcol2 cannot be.")
    }
    if (!labelcol1 %in% names(df1)) {
        message("labelcol1 not found in df1.")
    }
    if (!labelcol2 %in% names(df2)) {
        message("labelcol2 not found in df2.")
    }
    if (!is.null(joincol)) {
        if (!joincol %in% names(df1)) {
            message("joincol not found in df1.")
        }
        if (!joincol %in% names(df2)) {
            message("joincol not found in df2.")
        }
    } else {
        if (!length(intersect(names(df1), names(df2)))) {
            stop("no columns with equal names for joining in df1 and df2.")
        }
    }

    # actually a lot of checking needed but leave it for now
    # just record the idea

    # make sure only overlapping and correct order
    joined <- dplyr::inner_join(df1, df2, by = joincol)
    if (nrow(joined) == 0) {
        stop("no joined rows.")
    }
    message("nrow(df1): ", nrow(df1), ", nrow(df2): ", nrow(df2), ", nrow(joined): ", nrow(joined))
    compared <- brathering::compare_labels(joined[[labelcol1]], joined[[labelcol2]])

    # new columns with corresponding labels
    df1[[labelcol2]] <- compared[["row_corres"]][df1[[labelcol1]]]
    df2[[labelcol1]] <- compared[["col_corres"]][df2[[labelcol2]]]

    return(list(compared = compared,
                df1 = df1,
                df2 = df2))
}
