#' Pairwise jaccard indices
#'
#' @param x named list of vectors
#' @param return
#' @param calc_order
#' @param return_plot
#'
#' @return list of jaccard index formats and plot
#' @export
#'
#' @examples
#' a <- letters[1:5]
#' b <- letters[1:7]
#' c <- letters[5:10]
#' d <- letters[2:8]
#' x <- list(a=a,b=b,c=c,d=d)
#' jacdata <- jaccard_index_pairwise(x)
jaccard_index_pairwise <- function(x,
                                   return = c("all","upper_triangle", "lower_triangle", "no_diagonal",
                                              "upper", "lower", "no_diag"),
                                   calc_order = T,
                                   return_plot = T) {

    stopifnot("x must be a list" = is.list(x),
              "x needs names" = !is.null(names(x)))

    return <- rlang::arg_match(return)
    # # list of lists
    # jacinds <- purrr::map(x, ~purrr::map(x, jaccard_index, .x))
    #
    # # to data frame
    # jacinds2 <- purrr::map_dfr(stats::setNames(names(jacinds), names(jacinds)),
    #                            ~utils::stack(jacinds[[.x]]),
    #                            .id = "ind2")
    # jacinds2 <- jacinds2[,c("ind", "ind2", "values")]
    # names(jacinds2) <- c("xind", "yind", "jaccard_index")

    # filter(id1 < id2) |>
    # filter(id1 != id2) |>

    # data frame direct
    # df <- tidyr::crossing(
    #     xind = seq_along(x),
    #     yind = seq_along(x))
    #dplyr::mutate(jaccard = unlist(parallel::mcmapply(a = x[xind], b = x[yind], FUN = jaccard_index, mc.cores = mc.cores)))
    #dplyr::mutate(jaccard = purrr::map2_dbl(xind, yind, ~jaccard_index(x[[.x]], x[[.y]])))

    ## so much slower
    # df <- tibble::as_tibble(t(combn(seq_along(x), 2)), .name_repair = ~c("xind","yind"))
    # df$jaccard <- unlist(parallel::mcmapply(a = x[df$xind], b = x[df$yind], FUN = jaccard_index, mc.cores = mc.cores))
    # df$x <- names(x)[df$xind]
    # df$y <- names(x)[df$yind]
    # df2 <- combn(seq_along(x), 2, simplify = F)
    # df2 <- parallel::mclapply(df2, function(y) jaccard_index(x[y[1]], x[y[2]]), mc.cores = mc.cores)

    jaccard <- jaccard_pairwise(x, as = "dense")

    df <- brathering::mat_to_df_long(jaccard, rownames_to = "x", colnames_to = "y", values_to = "jaccard")

    if (calc_order) {
        df <- fcexpr::heatmap_ordering(df, groups = "x", features = "y", values = "jaccard") |>
            dplyr::mutate(xind = as.factor(as.numeric(x)), yind = as.factor(as.numeric(y)))
    } else {
        df$xind <- factor(match(df$x, names(x)))
        df$yind <- factor(match(df$y, names(x)))
    }
    df <- df |>
        dplyr::arrange(x,y) |>
        dplyr::mutate(y2 = paste0(y, " (", yind, ")"))
    df$y2 <- factor(df$y2, levels = dplyr::distinct(df, yind, y2)[["y2"]])

    #compfun <- `<`
    if (return %in% c("upper_triangle", "upper")) {
        df <- df |> dplyr::filter(as.numeric(xind) < as.numeric(yind))
    } else if (return %in% c("lower_triangle", "lower")) {
        df <- df |> dplyr::filter(as.numeric(xind) > as.numeric(yind))
    } else if (return %in% c("no_diagonal", "no_diag")) {
        df <- df |> dplyr::filter(as.numeric(xind) != as.numeric(yind))
    }

    plot <- NULL
    if (return_plot) {
        plot <- fcexpr::heatmap_long_df(df, groups = "xind", features = "y", values = "jaccard", values_zscored = F,
                                        heatmap_ordering_args = list(feature_order = "none", group_order = "none")) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())

    }

    return(list(df = df,
                mat = brathering::df_long_to_mat(df, to_rows = "x", to_cols = "y", values = "jaccard"),
                plot = plot))
}

if(base::getRversion() >= "2.15.1")  utils::globalVariables(c("xind", "yind"))
