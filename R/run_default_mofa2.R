#' Run MOFA2 default
#'
#' @param list_of_matrices must be feature x sample matrix; so samples (donors) as
#' columns
#' @param transpose if matrices are sample x feature as for PCA, transpose them?
#' @param outfile save path for mofa model
#' @param interactive_cluster_select get asked about best looking clustering?
#'
#' @returns MOFA model object and plots from analysis of single matrices
#' @export
#'
#' @examples
#' \dontrun{
#' space <- hdos::random_space(n_spheres = 8, n_tori = 0, n_cuboids = 0, max_dim = 10, n_samples = 20)
#' # 3 matrices with equal info
#' s1 = as.matrix(space$coord)
#' s2 <- s1 + matrix(rnorm(n = ncol(s1)*nrow(s1), sd = 0.5), ncol = ncol(s1))
#' s3 <- s1 + matrix(rnorm(n = ncol(s1)*nrow(s1), sd = 0.8), ncol = ncol(s1))
#' liofma <- list(space1 = s1, space2 = s2, space3 = s3)
#' mo <- run_default_mofa2(liofma, transpose = T)
#'}
run_default_mofa2 <- function(list_of_matrices,
                              transpose = F,
                              outfile = file.path(getwd(), "MOFA2_object.hdf5"),
                              interactive_cluster_select = T) {

    # system("which python", intern = T)
    # system("pip show mofapy2", intern = T)
    # reticulate::py_config()
    # reticulate::use_python("/opt/local/bin/python3", required = TRUE)

    if (transpose) {
        message("transposing.")
        list_of_matrices <- purrr::map(list_of_matrices, Matrix::t)
    }

    list_of_matrices <- check_matrices(liofma = list_of_matrices)
    inspect <- inspect_matrices(liofma = list_of_matrices,
                              interactive_cluster_select = interactive_cluster_select)

    ## run mofa
    mo <- MOFA2::create_mofa(list_of_matrices)

    model_opts <- MOFA2::get_default_model_options(mo)
    train_opts <- MOFA2::get_default_training_options(mo)
    data_opts <- MOFA2::get_default_data_options(mo)

    mo <- MOFA2::prepare_mofa(mo,
                              data_options = data_opts,
                              model_options = model_opts,
                              training_options = train_opts)
    mo <- MOFA2::run_mofa(mo, outfile = outfile)
    # MOFA2::plot_factors(mo, factors = c(1,2))
    # MOFA2::plot_variance_explained(mo)
    return(list(single_inspect = inspect,
                mofa = mo))
}

plot_eigen <- function(x, title = "", max_pc = 10) {
    x <- dplyr::slice_max(x, order_by = variance, n = max_pc, by = view, with_ties = F)
    ggplot2::ggplot(x, ggplot2::aes(x = pc, y = variance, color = view)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::labs(subtitle = title)
}

plot_loadings <- function(pca_prcomp, title, limits = c(-1,1)) {

    #loadings <- purrr::map(pcas, ~.x$rotation)
    loadings <- pca_prcomp$rotation
    x <- loadings[,1:min(5,ncol(loadings))]
    pc1_order <- names(sort(x[,1]))
    x <- brathering::mat_to_df_long(x = x,
                                    colnames_to = "PC",
                                    rownames_to = "feature",
                                    values_to = "loading")
    feats <- x |>
        dplyr::slice_max(order_by = abs(loading), n = 6, by = PC) |>
        dplyr::pull(feature)
    x <- x |> dplyr::filter(feature %in% feats)
    pc1_order <- pc1_order[which(pc1_order %in% x$feature)]
    x$feature <- factor(x$feature, levels = pc1_order)
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = PC, y = feature, fill = loading)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradientn(colors = colrr::col_pal("RdBu", direction = -1),
                                      limits = limits) +
        colrr::theme_material(white = T) +
        ggplot2::labs(subtitle = title)
    return(plot)

}

plot_pca_vars <- function(prcomp_pca, cluster_df = NULL, title = "") {

    if (is.null(cluster_df)) {
        ggplot2::ggplot(prcomp_pca$x, ggplot2::aes(x = PC1, y = PC2)) +
            ggplot2::geom_point() +
            ggplot2::labs(subtitle = title)
    } else {
        ggplot2::ggplot(cbind(prcomp_pca$x, cluster_df), ggplot2::aes(x = PC1, y = PC2, color = !!rlang::sym(colnames(cluster_df)[1]))) +
            ggplot2::geom_point() +
            ggplot2::labs(subtitle = title)
    }

}


plot_silhoutte <- function(clusters, dist, title) {

    if (length(unique(clusters)) == 1) {
        return(NULL)
    }
    sil <- cluster::silhouette(x = clusters, dist = dist)
    sil_df <- dplyr::mutate(dplyr::mutate(dplyr::arrange(as.data.frame(sil),
                                                         cluster, dplyr::desc(sil_width)), row = dplyr::row_number()),
                            cluster = as.character(cluster))
    theme <- colrr::theme_material(white = T)
    col_pal <- colrr::col_pal("custom")
    plot <- ggplot2::ggplot(sil_df, ggplot2::aes(x = row, y = sil_width)) +
        ggplot2::geom_col(ggplot2::aes(color = cluster, fill = cluster)) +
        ggplot2::geom_hline(yintercept = mean(sil_df$sil_width),
                            linetype = "dashed") + theme + ggplot2::scale_color_manual(values = col_pal) +
        ggplot2::labs(subtitle = title) +
        ggplot2::scale_fill_manual(values = col_pal) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
    return(plot)
}

inspect_matrices <- function(liofma,
                             interactive_cluster_select = F) {

    ## impute missing values, how?
    if (any(purrr::map_lgl(liofma, anyNA))) {
        message("missing values will be imputed by median imputation (very simple).")
        print(purrr::map_lgl(liofma, anyNA))

        liofma <- purrr::map(liofma, ~t(apply(.x, 1, function(row) {
            row[is.na(row)] <- median(row, na.rm = TRUE)
            row
        })))
    }

    ## compare variances, only makes sense when matrices were not scaled
    # matrices should be  feature x sample  by now
    message("sum of variances. consider data_opts$scale_views.")
    print(purrr::map_dbl(liofma, ~sum(apply(.x, 1, var))))

    # get feature loadings, which are feature weights, per PC
    # Positive loading: variable moves with the component
    # Negative loading: variable moves against the component
    # focus on top features per PC, just to get an idea of main source of variance
    # loadings from prcomp are always between −1 and 1
    pcas <- purrr::map(liofma, ~prcomp(t(.x), scale. = F))
    loadings_plot1 <- patchwork::wrap_plots(purrr::map2(pcas,
                                                        names(pcas),
                                                        ~plot_loadings(.x,.y, limits = c(-1,1))),
                                            guides = "collect", axes = "collect")
    print(loadings_plot1)
    loadings_plot2 <- patchwork::wrap_plots(purrr::map2(pcas,
                                                        names(pcas),
                                                        ~plot_loadings(.x,.y, limits = NULL)),
                                            axes = "collect")
    print(loadings_plot2)
    pcas2 <- purrr::map(liofma, ~prcomp(t(.x), scale. = T))

    # but total variance does not tell us about the structure, i.e. is variance isotropic (spread evenly across features) or low-dimensional (most variance in few directions)
    # so eigenvalues:
    # Using cov(X): eigenvalues = variance in original units
    # Using cor(X): eigenvalues = variance after standardization
    #raw_eigen <- dplyr::bind_rows(purrr::map(liofma, ~data.frame(variance = eigen(cov(t(.x)))$values, pc = 1:nrow(.x))), .id = "view")
    #scale_eigen <- dplyr::bind_rows(purrr::map(liofma, ~data.frame(variance = eigen(cor(t(.x)))$values, pc = 1:nrow(.x))), .id = "view")

    raw_eigen <- dplyr::bind_rows(purrr::map(pcas, ~data.frame(variance = .x$sdev^2, pc = 1:length(.x$sdev))), .id = "view")
    scale_eigen <- dplyr::bind_rows(purrr::map(pcas2, ~data.frame(variance = .x$sdev^2, pc = 1:length(.x$sdev))), .id = "view")

    eigen_plot <- patchwork::wrap_plots(plot_eigen(raw_eigen, title = "raw"),
                                        plot_eigen(scale_eigen, title = "scaled"),
                                        guides = "collect", axis_titles = "collect")
    print(eigen_plot)


    # cluster samples based on all features across views
    # worked better without scaling
    # anyway, there may be different valid clusters for different views, but just to get a common grouping for subsequent
    # liofma_scale <- purrr::map(liofma, brathering::row_scale)
    # matrix_scale <- dplyr::bind_rows(purrr::map(liofma_scale, as.data.frame))
    matrix <- dplyr::bind_rows(purrr::map(liofma, as.data.frame))
    matrix_dist <- stats::dist(t(matrix))

    cl <- fcexpr::get_louvain_cluster(t(matrix),
                                      FindClusters_args = list(resolution = seq(0.1,1.6,0.1)),
                                      mc.cores = 4)
    rownames(cl) <- colnames(matrix)
    cl <- cl[,which(apply(cl, 2, function(x) length(unique(x))) > 1)]

    sil_plot <- NULL
    cluster_df <- NULL

    if (ncol(cl) > 0) {
        # cl <- stats::setNames(as.data.frame(cl), "clusters") |> dplyr::mutate(clusters = as.character(clusters))
        # get unique clusterings
        hashes <- apply(cl, 2, digest::digest)
        cl_unique <- hashes[!duplicated(hashes)]
        cl <- cl[,names(cl_unique), drop = F]
        cl <- purrr::map(brathering::split_mat(cl, colnames(cl), byrow = F), ~.x[,1])

        sil_plot <- patchwork::wrap_plots(purrr::map2(cl, names(cl),
                                                      ~plot_silhoutte(clusters = .x, dist = matrix_dist, title = .y)))
        print(sil_plot)

        if (length(cl_unique) > 1 && interactive_cluster_select) {
            choice <- menu(names(cl_unique), title = "Pick a clustering")
            cl_choice <- names(cl_unique[choice])
        } else if (length(cl_unique) > 1 && !interactive_cluster_select) {
            cl_choice <- names(cl_unique)[ceiling(length(cl_unique)/2)] # midpoint
        } else {
            cl_choice <- names(cl_unique)[1]
        }
        cluster_df <- as.data.frame(cl[cl_choice,drop = F])
        cluster_df[[1]] <- as.character(cluster_df[[1]])
    }

    vars_plot <- patchwork::wrap_plots(purrr::map2(pcas, names(pcas), ~plot_pca_vars(prcomp_pca = .x,
                                                                                     cluster_df = cluster_df,
                                                                                     title = .y)),
                                       guides = "collect", axes = "collect")
    print(vars_plot)

    return(list(data = list(pcas_raw = pcas,
                            pcas_scale = pcas2,
                            clusters = cluster_df),
                plots = list(eigen_plot = eigen_plot,
                             loadings_plot1 = loadings_plot1,
                             loadings_plot2 = loadings_plot2,
                             sil_plot = sil_plot,
                             vars_plot = vars_plot)))
}

check_matrices <- function(liofma) {

    # check number of cols
    if (length(unique(purrr::map_int(liofma, ncol))) > 1) {
        print(purrr::map_int(liofma, ncol))
        stop("Different number of columns per matrix (view).")
    } else {
        print(purrr::map_chr(liofma, ~paste(dim(.x), collapse = " x ")))
    }

    # check intersecting colnames
    samples <- purrr::map(liofma, colnames)
    if (all(purrr::map_lgl(samples, is.null))) {
        message("all colnames NULL.")
        liofma <- purrr::map(liofma, function(x) {
            colnames(x) <- paste0("sample_", brathering::pad_num_in_str(as.character(1:ncol(x))))
            return(x)
        })
    } else if (is.null(purrr::reduce(samples, intersect)) || length(purrr::reduce(samples, intersect)) < length(samples[[1]])) {
        stop("colnames do not intersect.")
    }

    # equally order of columns by name (silently)
    liofma <- c(liofma[1],
                purrr::map(liofma[-1], ~.x[,colnames(liofma[[1]])]))


    # fix names of views if needed
    names_default <- paste0("view", brathering::pad_num_in_str(x = as.character(1:length(liofma)),
                                                               len = max(2,nchar(length(liofma)))))
    if (is.null(names(liofma))) {
        names(liofma) <- names_default
    }
    names(liofma)[which(is.na(names(liofma)))] <- names_default[which(is.na(names(liofma)))]
    names(liofma)[which(names(liofma) == "")] <- names_default[which(names(liofma) == "")]
    names(liofma) <- make.unique(names(liofma))

    # unify feature names
    # if only one is duplicated, attach view name to all, simpler
    # MOFA2:::.rename_duplicated_features()
    if (any(table(purrr::list_c(purrr::map(liofma, rownames))) > 2)) {
        liofma <- purrr::map(purrr::set_names(names(liofma)), function(x) {
            rownames(liofma[[x]]) <- paste0(rownames(liofma[[x]]), "_", x)
            return(liofma[[x]])
        })
    }
    return(liofma)
}
