#' Compare two vectors of labels for overlap/correspondence/similarity
#'
#' x and y may be two label vectors for same observations. compare how one
#' splits in the other. In return: raw is pure counts of shared obs; row_props
#' and col_probs tells relative sharing: e.g row1 of row_props tells the relative
#' split of label of row1 to collabels, col_probs: same from column perspective.
#' jaccard is self-explaining. row or colwise max values in row_props or
#' col_props are row_corres and col_corres. These may be used to transfer
#' labels.
#'
#' @param x vector 1 or data frame; if df then col1 and col2 become x and y
#' @param y vector 2
#' @param return return tables as matrices or 2D-tables
#'
#' @returns list
#' @export
#'
#' @examples
#' x <- data.frame(
#'     label1 = c("A","A","A","B","B","C","C","C","C"),
#'     label2 = c("X","X","Y","Y","Z","X","Z","Z","Z")
#' )
#' compare_labels(x = x)
compare_labels <- function(x,
                           y,
                           return = c("matrix", "table")) {

    # https://github.com/lazappi/clustree
    # https://github.com/crazyhottommy/scclusteval

    return <- rlang::arg_match(return)

    if (is.data.frame(x)) {
        y <- x[,2,drop=T]
        x <- x[,1,drop=T]
    }
    # else: vectors
    x <- as.character(x)
    y <- as.character(y)

    tab <- table(x, y)

    # For each col1 label: distribution over col2 (rows sum to 1)
    row_props <- prop.table(tab, margin = 1)

    # For each col2 label: distribution over col1 (columns sum to 1)
    col_props <- prop.table(tab, margin = 2)

    # 2) Jaccard matrix between labels in col1 (rows) and col2 (cols)
    # J_ij = n_ij / (n_i + m_j - n_ij)
    n_i <- rowSums(tab)
    m_j <- colSums(tab)
    den <- outer(n_i, m_j, "+") - tab
    jaccard_mat <- tab / den
    jaccard_mat[is.na(jaccard_mat)] <- 0  # just in case of zeros

    # correspondence: top shared label
    # can be used to transfer labels
    row_corres <- stats::setNames(colnames(row_props)[apply(row_props, 1, which.max)], rownames(row_props))
    col_corres <- stats::setNames(rownames(col_props)[apply(col_props, 2, which.max)], colnames(col_props))

    # check for ties of max
    # these would question label correspondence; first max is returned
    row_tie <- apply(row_props, 1, function(x) sum(x == max(x)) > 1)
    col_tie <- apply(col_props, 2, function(x) sum(x == max(x)) > 1)
    if (any(row_tie)) {
        message(sum(row_tie), " rowwise max values of row_props are ties.")
        print(row_tie)
    }
    if (any(col_tie)) {
        message(sum(col_tie), " colwise max values of col_props are ties.")
        print(col_tie)
    }

    # make matrices from tables
    if (return == "matrix") {
        tab <- matrix(
          data = as.vector(tab),
          nrow = nrow(tab),
          ncol = ncol(tab),
          dimnames = list(rownames(tab), colnames(tab)))
        row_props <- matrix(
            data = as.vector(row_props),
            nrow = nrow(row_props),
            ncol = ncol(row_props),
            dimnames = list(rownames(row_props), colnames(row_props)))
        col_props <- matrix(
            data = as.vector(col_props),
            nrow = nrow(col_props),
            ncol = ncol(col_props),
            dimnames = list(rownames(col_props), colnames(col_props)))
        jaccard <- matrix(
            data = as.vector(jaccard),
            nrow = nrow(jaccard),
            ncol = ncol(jaccard),
            dimnames = list(rownames(jaccard), colnames(jaccard)))
    }

    return(list(raw = tab,
                row_props = row_props,
                col_props = col_props,
                jaccard = jaccard_mat,
                row_corres = row_corres,
                col_corres = col_corres))
}

