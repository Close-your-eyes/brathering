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
                           y) {

    # https://github.com/lazappi/clustree
    # https://github.com/crazyhottommy/scclusteval

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
    jaccard <- tab / den
    jaccard[is.na(jaccard)] <- 0  # just in case of zeros

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
    tab <- make_matrix(tab)
    row_props <- make_matrix(row_props)
    col_props <- make_matrix(col_props)
    jaccard <- make_matrix(jaccard)

    col_props <- adjust_order_make_df(col_props, legend_name = "x in y")
    row_props <- adjust_order_make_df(row_props, legend_name = "y in x")
    tab <- adjust_order_make_df(tab, legend_name = "shared (n)")
    jaccard <- adjust_order_make_df(jaccard, legend_name = "jaccard\nindex")

    return(list(raw = tab,
                row_props = row_props,
                col_props = col_props,
                jaccard = jaccard,
                row_corres = row_corres,
                col_corres = col_corres))
}

make_matrix <- function(m) {
    m <- matrix(
        data = as.vector(m),
        nrow = nrow(m),
        ncol = ncol(m),
        dimnames = dimnames(m))
    return(m)
}

adjust_order_make_df <- function(mat, legend_name) {
    df <- brathering::mat_to_df_long(x = mat,
                                     rownames_to = "y",
                                     colnames_to = "x")
    df <- fcexpr::heatmap_ordering(
        df = df,
        features = "x",
        groups = "y",
        values = "value",
        feature_order = "custom",
        group_order = "hclust")
    mat <- mat[rev(levels(df[["y"]])), levels(df[["x"]])]

    plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_tile(ggplot2::aes(fill = value), color = "black") +
        colrr::scale_fill_spectral() +
        ggplot2::labs(fill = legend_name) +
        ggplot2::geom_text(data = dplyr::filter(df, value >= 0.05), ggplot2::aes(label = round(value, 2)))

    return(list(mat = mat, df = df, plot = plot))
}
