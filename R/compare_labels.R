#' Compare two label vectors for overlap, correspondence, and similarity
#'
#' Compares two label vectors, `x` and `y`, describing the same observations.
#' The function summarizes how labels in `x` split across labels in `y`, and
#' vice versa.
#'
#' If `x` is a data frame or matrix, its first two columns are used as `x` and
#' `y`. Missing pairs are removed before comparison.
#'
#' The returned `raw`, `x_probs`, `y_probs`, and `jaccard` entries each contain
#' a matrix, a long-format data frame, and a heatmap plot. `raw` contains shared
#' observation counts. `x_probs` contains the distribution of each `x` label
#' across `y` labels. `y_probs` contains the distribution of each `y` label
#' across `x` labels. `jaccard` contains pairwise Jaccard indices between labels
#' in `x` and labels in `y`.
#'
#' `x_corres` gives the best-matching `y` label for each `x` label, based on
#' the maximum value in `x_probs`. `y_corres` gives the best-matching `x` label
#' for each `y` label, based on the maximum value in `y_probs`. Ties are reported
#' by message and the first maximum is used.
#'
#' Joined labels are returned in `join_labels`. Low-frequency joined labels,
#' defined by `join_label_thresh`, are reassigned to the most frequent joined
#' label within the corresponding parent label.
#'
#' Overall similarity (global_comp)
#' Detailed overlap (raw)
#' Conditional relationships (x_probs, y_probs)
#' Pairwise similarity (jaccard)
#' Best correspondences (x_corres, y_corres)
#' Utilities for relabeling (join_labels)
#'
#' @param x A label vector, data frame, or matrix. If a data frame or matrix,
#'   the first column is used as `x` and the second column as `y`.
#' @param y A second label vector. Ignored when `x` is a data frame or matrix.
#' @param freq_label_cutoff Numeric cutoff used for displaying text labels in
#'   heatmap plots. Values less than or equal to this cutoff are not printed
#'   inside tiles.
#' @param join_label_sep Character string used to join `x` and `y` labels.
#' @param join_label_thresh Numeric threshold for joined-label frequencies.
#'   Joined labels with relative frequency below this threshold are reassigned
#'   to the most frequent joined label within the same parent label.
#'
#' @returns A list with entries:
#' \describe{
#'   \item{raw}{Shared observation counts between `x` and `y` labels.}
#'   \item{x_probs}{Conditional proportions of `y` labels within each `x` label.}
#'   \item{y_probs}{Conditional proportions of `x` labels within each `y` label.}
#'   \item{jaccard}{Pairwise Jaccard index between `x` and `y` labels.}
#'   \item{x_corres}{Best-matching `y` label for each `x` label.}
#'   \item{y_corres}{Best-matching `x` label for each `y` label.}
#'   \item{join_labels}{Joined and frequency-corrected joined labels in both
#'     `xy` and `yx` directions.}
#' }
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'     label1 = c("A","A","A","B","B","C","C","C","C"),
#'     label2 = c("X","X","Y","Y","Z","X","Z","Z","Z")
#' )
#'
#' out <- compare_labels(x = x)
#' out$x_corres
#' out$y_corres
#'
#' # global_comp: A panel of complementary similarity measures between two clusterings.
#' # They fall into three broad categories: pair-counting, information-theoretic, and contingency-table measures.
#' # | Metric | Range | Higher/lower | What it measures                   | Sensitive to splitting? |
#' # | ------ | ----: | :----------: | ---------------------------------- | :---------------------: |
#' # | RI     |   0–1 |    Higher    | Pairwise agreement                 |          Weakly         |
#' # | ARI    | ~−1–1 |    Higher    | Chance-adjusted pairwise agreement |           Yes           |
#' # | MARI   | ~−1–1 |    Higher    | Modified ARI                       |           Yes           |
#' # | NMI    |   0–1 |    Higher    | Shared information                 |        Moderately       |
#' # | AMI    |  ~0–1 |    Higher    | Chance-adjusted mutual information |        Moderately       |
#' # | NVI    |   0–1 |     Lower    | Normalized information loss        |           Yes           |
#' # | NID    |   0–1 |     Lower    | Information distance               |           Yes           |
#' # | Chi²   |    ≥0 |    Higher    | Dependence between partitions      |      Not normalized     |
compare_labels <- function(x,
                           y,
                           adjust_order = T,
                           make_plots = T,
                           freq_label_cutoff = 0,
                           join_label_sep = "_",
                           join_label_thresh = 0.05) {

    # related:
    # https://github.com/lazappi/clustree
    # https://github.com/crazyhottommy/scclusteval
    # mclust::adjustedRandIndex()

    if (is.data.frame(x)) {
        message("x is df. using its first 2 columns for x and y.")
        y <- x[,2,drop=T]
        x <- x[,1,drop=T]
    } else if (is.matrix(x)) {
        message("x is matrix. using its first 2 columns for x and y.")
        y <- x[,2]
        x <- x[,1]
    } else {
        if (length(x) != length(y)) {
            stop("x and y must have the same length.")
        }
    }

    if (is.numeric(x) || is.numeric(y)) {
        stop("x or y is numeric. not allowed.")
    }

    # remove NA by default
    keep <- !is.na(x) & !is.na(y)
    nonnafreq <- mean(keep)
    if (nonnafreq == 0) {
        stop("no complete pairs left after NA removal.")
    } else if (nonnafreq < 1) {
        message("keeping ", round(nonnafreq, 2)*100, " % of complete pairs which are non-NA.")
    }

    x <- as.character(x[keep])
    y <- as.character(y[keep])

    global_comp <- aricode::compare_clustering(as.factor(x),
                                               as.factor(y))

    local_comp <- list(raw = table(x, y))
    local_comp[["x_props"]] <- prop.table(local_comp[["raw"]], margin = 1)
    local_comp[["y_props"]] <- prop.table(local_comp[["raw"]], margin = 2)
    local_comp[["jaccard"]] <- make_jaccard(local_comp[["raw"]])

    # correspondence: top shared label
    # can be used to transfer labels

    # catch error when one cluster does not have any match in other labels
    x_maxes <- max.col(local_comp[["x_props"]], ties.method = "first")
    names(x_maxes) <- rownames(local_comp[["x_props"]])
    #x_maxes[which(lengths(x_maxes) == 0)] <- NA

    y_maxes <- max.col(t(local_comp[["y_props"]]), ties.method = "first")
    names(y_maxes) <- colnames(local_comp[["y_props"]])
    #y_maxes[which(lengths(y_maxes) == 0)] <- NA

    x_corres <- stats::setNames(colnames(local_comp[["x_props"]])[x_maxes],
                                rownames(local_comp[["x_props"]]))
    y_corres <- stats::setNames(rownames(local_comp[["y_props"]])[y_maxes],
                                colnames(local_comp[["y_props"]]))

    # check for ties of max
    # these would question label correspondence; first max is returned
    x_tie <- apply(local_comp[["x_props"]], 1, function(x) sum(x == max(x)) > 1)
    y_tie <- apply(local_comp[["y_props"]], 2, function(x) sum(x == max(x)) > 1)
    if (any(x_tie, na.rm = T)) {
        message(sum(x_tie), " rowwise max values of x_props are ties.")
        print(x_tie)
    }
    if (any(y_tie, na.rm = T)) {
        message(sum(y_tie), " colwise max values of y_props are ties.")
        print(y_tie)
    }

    # make matrices from tables
    local_comp <- purrr::map(local_comp, make_matrix_and_df, adjust_order = adjust_order)

    if (make_plots) {
        local_comp <- purrr::map2(local_comp,
                                  c("shared (n)", "x in y", "y in x", "jaccard\nindex"),
                                  function(x,y) {
                                      x[["plot"]] <- make_plot(x[["df"]], legend_name = y,
                                                               freq_label_cutoff = freq_label_cutoff)
                                      return(x)
                                  })
    } else {
        local_comp <- purrr::map(local_comp, function(x) {
            x[["plot"]] <- NULL
            return(x)
        })
    }

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
                      xyfix = dplyr::if_else(rel < join_label_thresh, xyfix, xy),
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
                      yxfix = dplyr::if_else(rel < join_label_thresh, yxfix, yx),
                      .by = y) |>
        dplyr::distinct(y, yx, yxfix)
    yx_fix <- stats::setNames(yx_join_df$yxfix, yx_join_df$yx)
    yx_join_vec <- paste0(y, join_label_sep, x)
    yx_join_vec_fix <- unname(yx_fix[yx_join_vec])

    return(list(global_comp = global_comp,
                local_comp = local_comp,
                correspondence = list(x = x_corres,
                                      y = y_corres),
                join_labels = list(xy = list(df = xy_join_df,
                                             fix = xy_fix,
                                             join = xy_join_vec,
                                             join_fix = xy_join_vec_fix),
                                   yx = list(df = yx_join_df,
                                             fix = yx_fix,
                                             join = yx_join_vec,
                                             join_fix = yx_join_vec_fix))))
}

make_matrix_and_df <- function(m,
                               adjust_order = T) {

    mat <- matrix(
        data = as.vector(m),
        nrow = nrow(m),
        ncol = ncol(m),
        dimnames = dimnames(m))
    mat[which(is.na(mat))] <- 0

    # contingency table is kind-of flipped. fix this here for mat and df.
    mat <- t(mat)

    df <- brathering::mat_to_df_long(x = mat,
                                     rownames_to = "y",
                                     colnames_to = "x")
    if (adjust_order) {
        df <- fcexpr::heatmap_ordering(
            df = df,
            features = "x",
            groups = "y",
            values = "value",
            feature_order = "custom",
            group_order = "hclust")
        mat <- mat[rev(levels(df[["y"]])), levels(df[["x"]])]
    }

    return(list(mat = mat, df = df))
}

make_plot <- function(df,
                      legend_name,
                      freq_label_cutoff = 0) {

    plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_tile(ggplot2::aes(fill = value), color = "black") +
        colrr::scale_fill_spectral() +
        ggplot2::labs(fill = legend_name) +
        ggplot2::geom_text(data = dplyr::filter(df, value > freq_label_cutoff),
                           ggplot2::aes(label = round(value, 2))) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    return(plot)
}

make_jaccard <- function(raw) {
    # 2) Jaccard matrix between labels in col1 (rows) and col2 (cols)
    # J_ij = n_ij / (n_i + m_j - n_ij)
    n_i <- rowSums(raw)
    m_j <- colSums(raw)
    den <- outer(n_i, m_j, "+") - raw
    jaccard <- raw / den
    jaccard[is.na(jaccard)] <- 0  # just in case of zeros
    return(jaccard)
}

