#' Sampling with leverage-based probabilities
#'
#' Sample rows from a numeric matrix with leverage scores as probabilities.
#' This more like avoids that rare communities are lost due to sampling.
#'
#' @param x numeric matrix with observations as rows and features as columns
#' @param size see base::sample
#' @param replace see base::sample
#' @param leverage use leverage score as probabilities,
#' calculated by Seurat::LeverageScore
#' @param ... arguments to Seurat::LeverageScore
#' @param cluster pre-computed clusters of x, x is not needed when cluster is
#' provided
#' @param seed seed for replication of random results
#' @param n number of samples to draw
#'
#' @return numeric vector (sampled row indices)
#' @export
#'
#' @examples
#' # create clearly separated communities of different sizes
#' comm1 <- data.frame(x=rnorm(10000, mean=10), y=rnorm(10000, mean=10), z=rnorm(10000, mean=10))
#' comm2 <- data.frame(x=rnorm(1000, mean=20), y=rnorm(1000, mean=10), z=rnorm(1000, mean=0))
#' comm3 <- data.frame(x=rnorm(100, mean=-40), y=rnorm(100, mean=-10), z=rnorm(100, mean=-10))
#' comm4 <- data.frame(x=rnorm(20, mean=-10), y=rnorm(20, mean=-20), z=rnorm(20, mean=30))
#' comm_ident <- c(rep(c("A", "B", "C", "D"), sapply(list(comm1,comm2,comm3,comm4), nrow)))
#' all_comm <- cbind(do.call(rbind, list(comm1,comm2,comm3,comm4)),comm_ident)
#' # hdos::plotly3dplot(all_comm, color = "comm_ident")
#'
#' # calc their leverage
#' levscore <- Seurat::LeverageScore(t(all_comm[,1:3]))
#' # members of rare communities have higher leverage
#' mean(levscore[1:10000])
#' mean(levscore[10001:11000])
#' mean(levscore[11001:11100])
#' mean(levscore[11101:11110])
#'
#' # compare sampling
#' inds <- sample_leverage(x = as.matrix(all_comm[,1:3]), 100)
#' table(comm_ident[inds]) # sampled communities are similar in size
#' inds2 <- inds <- sample_leverage(as.matrix(all_comm[,1:3]), 100, leverage = F)
#' table(comm_ident[inds2]) # community D is lost and A dominates
#'
#' # compute cluster before
#' clst <- fcexpr::get_hclust_clusters(all_comm[,1:3], k = c(4,8,10))
#' rle(clst[,1]) # matches perfectly, k = 4
#' rle(clst[,2]) # k = 8
#' table(clst[,2])
#' inds3 <- sample_leverage(x = as.matrix(all_comm[,1:3]), 100, cluster = clst[,2])
#' table(comm_ident[inds3]) # wrong cluster number also yields appropriate results
sample_leverage <- function(x,
                            size = NULL,
                            replace = FALSE,
                            leverage = T,
                            cluster = NULL,
                            seed = 42,
                            n = 1,
                            ...) {

    stopifnot("x must a matrix." = is.matrix(x))

    if (is.null(size)) {
        size <- nrow(x)
        if (!replace) {
            rowinds <- 1:nrow(x)
            return(rowinds)
        }
    }

    prob <- NULL
    if (!is.null(cluster)) {
        # if clusters are known, use their inverse freq as prob
        stopifnot("cluster must be of length nrow(x)" = nrow(x) == length(cluster))
        if (leverage) {
            message("cluster is given, leverage is set F.")
            leverage <- F
        }
        ctable <- table(cluster)
        ccount <- stats::setNames(as.numeric(ctable), names(ctable))
        csum <- sum(ccount)
        cfreq <- ccount/csum
        prob <- 1/cfreq # probability as inverse of frequency
        prob <- prob/sum(prob)
        # add a little variation to probs
        set.seed(seed)
        prob_var <- mapply(rnorm, n = ccount, mean = prob, sd = prob/10, SIMPLIFY = F)
        prob <- prob[cluster]
        for (i in names(prob_var)) {
            prob[which(names(prob) == i)] <- prob_var[[i]]
        }
        rows <- length(cluster)
    }


    if (leverage) {
        if (!requireNamespace("Seurat", quietly = T)) {
            utils::install.packages("Seurat")
        }
        levscore <- Seurat::LeverageScore(t(x), ...)
        prob <- levscore/sum(levscore) # not necessary but anyways
        rows <- nrow(x)
    }

    set.seed(seed)
    rowinds <- replicate(n, sample(x = 1:rows, size = size, replace = replace, prob = prob), simplify = F)
    return(rowinds)
}


