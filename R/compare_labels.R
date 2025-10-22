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
#' @param freq_label_cutoff
#' @param join_label_sep
#' @param join_label_tresh
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
                           freq_label_cutoff = 0,
                           join_label_sep = "_",
                           join_label_tresh = 0.05) {

    # related:
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

    #purrr::map_int(apply(row_props, 1, c, simplify = F), which.max)
    #purrr::map_int(apply(col_props, 2, c, simplify = F), which.max)

    # catch error when one cluster does not have any match in other labels
    row_maxes <- apply(row_props, 1, which.max)
    row_maxes[which(lengths(row_maxes) == 0)] <- NA
    row_maxes <- stats::setNames(unlist(row_maxes), names(row_maxes))

    col_maxes <- apply(col_props, 2, which.max)
    col_maxes[which(lengths(col_maxes) == 0)] <- NA
    col_maxes <- stats::setNames(unlist(col_maxes), names(col_maxes))

    row_corres <- stats::setNames(colnames(row_props)[row_maxes], rownames(row_props))
    col_corres <- stats::setNames(rownames(col_props)[col_maxes], colnames(col_props))

    # check for ties of max
    # these would question label correspondence; first max is returned
    row_tie <- apply(row_props, 1, function(x) sum(x == max(x)) > 1)
    col_tie <- apply(col_props, 2, function(x) sum(x == max(x)) > 1)
    if (any(row_tie, na.rm = T)) {
        message(sum(row_tie), " rowwise max values of row_props are ties.")
        print(row_tie)
    }
    if (any(col_tie, na.rm = T)) {
        message(sum(col_tie), " colwise max values of col_props are ties.")
        print(col_tie)
    }

    # make matrices from tables
    tab <- make_matrix(tab)
    row_props <- make_matrix(row_props)
    col_props <- make_matrix(col_props)
    jaccard <- make_matrix(jaccard)

    col_props <- adjust_order_make_df(col_props, legend_name = "x in y", freq_label_cutoff = freq_label_cutoff)
    row_props <- adjust_order_make_df(row_props, legend_name = "y in x", freq_label_cutoff = freq_label_cutoff)
    tab <- adjust_order_make_df(tab, legend_name = "shared (n)", freq_label_cutoff = freq_label_cutoff)
    jaccard <- adjust_order_make_df(jaccard, legend_name = "jaccard\nindex", freq_label_cutoff = freq_label_cutoff)

    ## join labels:
    # in fix: groups below tresh are assigned to top freq group
    # is there another way to assign groups below tresh?
    xy_join_df <- data.frame(x,y) |>
        dplyr::mutate(xy = paste0(x, join_label_sep, y)) |>
        tibble::as_tibble() |>
        dplyr::count(x, xy, name = "n") |>
        dplyr::mutate(total = sum(n),
                      rel = n / total,
                      xyfix = xy[which.max(n)],
                      xyfix = dplyr::if_else(rel < join_label_tresh, xyfix, xy),
                      .by = x) |>
        dplyr::distinct(x, xy, xyfix)
    xy_fix <- stats::setNames(xy_join_df$xyfix, xy_join_df$xy)
    xy_join_vec <- paste0(x, join_label_sep, y)
    xy_join_vec_fix <- unname(xy_fix[xy_join_vec])

    yx_join_df <- data.frame(y,x) |>
        dplyr::mutate(yx = paste0(y, join_label_sep, x)) |>
        tibble::as_tibble() |>
        dplyr::count(y, yx, name = "n") |>
        dplyr::mutate(total = sum(n),
                      rel = n / total,
                      yxfix = yx[which.max(n)],
                      yxfix = dplyr::if_else(rel < join_label_tresh, yxfix, yx),
                      .by = y) |>
        dplyr::distinct(y, yx, yxfix)
    yx_fix <- stats::setNames(yx_join_df$yxfix, yx_join_df$yx)
    yx_join_vec <- paste0(y, join_label_sep, x)
    yx_join_vec_fix <- unname(yx_fix[yx_join_vec])

    ## long version of join labels:
    # jj <- paste0(hinze@meta.data$celltype, "_", as.character(hinze@meta.data$integrated_snn_res.0.4))
    # df1 <- data.frame(celltype = hinze@meta.data$celltype,
    #                   join = jj)
    # df11 <- df1 |>
    #   dplyr::count(dplyr::pick(dplyr::everything())) |>
    #   dplyr::left_join(dplyr::count(df1, celltype) |> dplyr::rename("total" = n)) |>
    #   dplyr::mutate(rel = n/total)
    #
    # df11maxgroup <- df11 |>
    #   dplyr::filter(n == max(n), .by = celltype) |>
    #   dplyr::select(celltype, join) |>
    #   dplyr::rename("joinfix" = join) |>
    #   dplyr::select(celltype, joinfix)
    #
    # df11fix <- df11 |>
    #   dplyr::left_join(df11maxgroup, by = "celltype") |>
    #   dplyr::mutate(joinfix = ifelse(rel < tresh, joinfix, join)) |>
    #   dplyr::distinct(celltype, join, joinfix)

    return(list(raw = tab,
                row_props = row_props,
                col_props = col_props,
                jaccard = jaccard,
                row_corres = row_corres,
                col_corres = col_corres,
                join_labels = list(xy = list(df = xy_join_df,
                                             fix = xy_fix,
                                             join = xy_join_vec,
                                             join_fix = xy_join_vec_fix),
                                   yx = list(df = yx_join_df,
                                             fix = yx_fix,
                                             join = yx_join_vec,
                                             join_fix = yx_join_vec_fix))))
}

make_matrix <- function(m) {
    m <- matrix(
        data = as.vector(m),
        nrow = nrow(m),
        ncol = ncol(m),
        dimnames = dimnames(m))
    m[which(is.na(m))] <- 0
    return(m)
}

adjust_order_make_df <- function(mat,
                                 legend_name,
                                 freq_label_cutoff = 0) {

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
        ggplot2::geom_text(data = dplyr::filter(df, value > freq_label_cutoff),
                           ggplot2::aes(label = round(value, 2)))

    return(list(mat = mat, df = df, plot = plot))
}
