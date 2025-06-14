#' Outlier detection in high dimensional space
#'
#' For Seurat leverage score is calculated. For mclust cluster identities are
#' calculated followed by heuristic for outlier decision. Leverage score is
#' fast but may fail with very few observations.
#' Returned columns:
#' hdout: HDoutliers::HDoutliers
#' db: dbscan::dbsca
#' lvs: Seurat::LeverageScore
#' mah: stats::mahalanobis
#' lof: Rlof::lof
#' mv: mvoutlier::pcout
#' rb: robustbase::covMcd
#' mcl: mclust::Mclust
#'
#' @param df data frame or matrix with numeric columns only
#' @param pca run pca on df?
#' @param pcs number of components for pca
#' @param methods methods for outlier detection
#' @param return instead of outlier classification, return numeric result from
#' algorithm where possible (Seurat, mahalanobis, Rlof, mclust); for mclust
#' classification
#'
#' @return matrix with cols being results from different outlier estimations
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- brathering:::generate_datasets(row_powers = 4, col_seq = 10)[[1]]
#' um <- fcexpr::ff_calc_umap_tsne(exprs = as.matrix(df))
#' pc <- fcexpr::ff_calc_pca(exprs = as.matrix(df))[["x"]][,c(1,2)]
#' outmat <- brathering::outlier(df)
#'
#' dfres <- dplyr::bind_cols(as.data.frame(um), as.data.frame(pc), as.data.frame(outmat))
#' dfres <- tidyr::pivot_longer(dfres, -c(UMAP_1, UMAP_2, PC1, PC2), names_to = "method", values_to = "outlier")
#'
#' ggplot(dfres, aes(UMAP_1, UMAP_2)) +
#'     geom_point(aes(color = as.factor(outlier))) +
#'     facet_wrap(vars(method))
#'
#' ggplot(dfres, aes(PC1, PC2)) +
#'     geom_point(aes(color = as.factor(outlier))) +
#'     facet_wrap(vars(method))
outlier <- function(df,
                    pca = T,
                    pcs = 2,
                    return = c("outlier", "numeric"),
                    methods = c("HDoutliers", "dbscan", "Seurat", "mahalanobis", "Rlof", "mvoutlier", "robustbase", "mclust")) {

    return <- rlang::arg_match(return)
    methods <- rlang::arg_match(methods, multiple = T)

    to_install <- intersect(c("HDoutliers", "dbscan", "Seurat", "Rlof", "mvoutlier", "robustbase", "mclust"), methods)
    for (i in to_install) {
        if (!requireNamespace(i, quietly = T)) {
            utils::install.packages(i)
        }
    }

    if (pca && pcs < ncol(df)) {
        df <- stats::prcomp(df)[["x"]][,c(1:pcs)]
    } else {
        df <- as.matrix(df)
    }

    ## 1
    hdout <- NULL
    if ("HDoutliers" %in% methods) {
        hdout <- HDoutliers::HDoutliers(df)
        hdout <- as.numeric(1:nrow(df) %in% hdout)
    }

    ## 2
    db <- NULL
    if ("dbscan" %in% methods) {
        db <- dbscan::dbscan(df, eps = 0.5)
        db <- as.numeric(db[["cluster"]] == 0)
    }

    ## 3
    lvs <- NULL
    if ("Seurat" %in% methods) {
        try(expr = {
            lvs <- Seurat::LeverageScore(t(as.matrix(df)))
            if (return == "outlier") {
                lvs <- as.numeric(outlier1d(lvs, return = "vec", minsum = 3))
            }
            #lvsquant <- quantile(lvs, 0)
        }, silent = T)
    }
    ## 4
    mah <- NULL
    if ("mahalanobis" %in% methods) {
        mah <- stats::mahalanobis(df, colMeans(df), stats::cov(df))
        if (return == "outlier") {
            mah <- as.numeric(mah >= qchisq(0.99, df = 2))
        }
    }

    ## 5
    lof <- NULL
    if ("Rlof" %in% methods) {
        lof <- Rlof::lof(df, k = min(c(50, nrow(df)/2)))
        if (return == "outlier") {
            lof <- as.numeric(lof > 1.5)
        }
    }

    ## 6
    #pr <- stats::prcomp(df, scale. = T)
    #pr <- as.numeric(outlier1d(x = pr$x[,1], return = "vec", minsum = 3))

    ## 7
    mv <- NULL
    if ("mvoutlier" %in% methods) {
        mv <- mvoutlier::pcout(df, outbound = 0.04)
        mv <- as.numeric(mv$wfinal01 != 1)
    }

    ## 8
    rb <- NULL
    if ("robustbase" %in% methods) {
        rb <- robustbase::covMcd(df, nsamp = 1000)
        rb <- as.numeric(rb$mcd.wt != 1)
    }

    ## 9
    mcl <- NULL
    if ("mclust" %in% methods) {
        dtach <- !"mclust" %in% .packages()
        library(mclust)
        mcl <- mclust::Mclust(df)
        mclt <- table(mcl[["classification"]])
        if (return == "outlier") {
            fractions <- mclt/nrow(df)
            if (any(fractions < 0.2)) {
                # max. 20 % outliers possible
                # many groups of similar size --> no outlier
                mcl <- 0 # no outliers
            } else if (any(fractions > 0.8)) {
                # when there is one large group, the other groups are outlier
                mcl <- as.numeric(mcl[["classification"]] != names(which.max(mclt)))
            } else if (any(fractions <= 0.1)) {
                # small groups are outliers
                mcl <- as.numeric(mcl[["classification"]] != names(mclt)[which(fractions <= 0.1)])
            } else {
                # the smallest group is outlier
                mcl <- as.numeric(mcl[["classification"]] == names(which.min(mclt)))
            }
        } else {
            mcl <- mcl[["classification"]]
        }
        if (dtach) {
            detach("package:mclust", unload = T)
        }
    }

    return(cbind(hdout, db, lvs, mah, lof, mv, rb, mcl))
}


#' Simple methods for outlier detection in 1D
#'
#' Three methods for outlier detection in 1 dimension:
#' 1st and 3rd quartile +/- 1.5*IQR, zscore > 1.5, hampel filter using
#' median +/- 3*MAD. When return is mat results from all methods are returned.
#' If return is vec results are collapsed by summation and minsum becomes
#' relevant.
#'
#' @param x numeric vector
#' @param return type to return
#' @param minsum 1,2 or 3: minimum number of methods
#' indicating an outlier, only if return is vec
#'
#' @return matrix or vector
#' @export
#'
#' @examples
#' x <- c(rnorm(1000), rnorm(20,mean = 10))
#' outlier1d(x, minsum = 3)
outlier1d <- function(x, return = c("vec", "mat"), minsum = 3) {
    return <- rlang::arg_match(return)

    mat <- as.matrix(data.frame(IQR = IQRout(x),
                                zscore = zscore(x),
                                hampel = hampel(x)))
    if (return == "mat") {
        return(mat)
    } else {
        return(apply(mat, 1, function(y) sum(y) >= minsum))
    }
}



IQRout <- function(x, fact = 1.5) {
    iqr <- stats::IQR(x)
    quants <- stats::quantile(x, c(0.25, 0.75))
    outlier <- as.numeric(x < (quants[1] - 1.5*iqr) | x > (quants[2] + 1.5*iqr))
    return(outlier)
}

hampel <- function(x, fact = 3) {
    lb <- median(x) - fact * mad(x, constant = 1)
    ub <- median(x) + fact * mad(x, constant = 1)
    outlier <- as.numeric(x < lb | x > ub)
    return(outlier)
}

zscore <- function(x, fact = 1.5) {
    outlier <- as.numeric(abs(scale(x)[,1]) > fact)
    return(outlier)
}


generate_datasets <- function(row_powers = c(2,4,6),
                              col_seq = c(2, 10, 50),
                              outlier_frac = 0.01, seed = 123) {
    set.seed(seed)
    datasets <- list()

    for (r_power in row_powers) {
        n_rows <- 10^r_power
        for (n_cols in col_seq) {
            df_name <- paste0("df_r", n_rows, "_c", n_cols)

            # Generate normal data
            df <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)

            # Inject outliers
            n_outliers <- ceiling(outlier_frac * n_rows)
            if (n_outliers > 0) {
                for (i in 1:n_outliers) {
                    row_idx <- sample(1:n_rows, 1)
                    col_idx <- sample(1:n_cols, 1)
                    df[row_idx, col_idx] <- df[row_idx, col_idx] + rnorm(1, mean = 10, sd = 5)  # Large shift
                }
            }

            df <- as.data.frame(df)
            colnames(df) <- paste0("V", 1:n_cols)
            datasets[[df_name]] <- df
        }
    }
    return(datasets)
}


