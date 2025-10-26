#' Kendall's tau for zero-inflated count data between columns of two matrices
#'
#' Extension of dismay::kendall_zi.
#'
#' @param mat1 matrix 1
#' @param mat2 matrix 2
#' @param mc.cores parallel computing?
#'
#' @returns matrix of correlation indices
#' @export
#'
#' @examples
kendall_zi_cross <- function(mat1, mat2, mc.cores = 1) {

    if (!requireNamespace("dismay", quietly = T)) {
        devtools::install_github("skinnider/dismay")
    }

    allcombdf <- dplyr::filter(expand.grid(colnames(mat1), colnames(mat2)), Var1 != Var2)
    allcomb <- asplit(allcombdf, 1)
    pair_corr <- parallel::mclapply(allcomb,
                                    function(x) dismay::kendall_zi(cbind(mat1[,x[1],drop=F], mat2[,x[2],drop=F])),
                                    mc.cores = mc.cores)
    corr_df <- purrr::map(pair_corr, brathering::mat_to_df_long) |>
        dplyr::bind_rows() |>
        dplyr::distinct(rname, cname, .keep_all = T)
    corr_mat <- brathering::df_long_to_mat(df = corr_df,
                                           to_rows = "rname",
                                           to_cols = "cname",
                                           values = "value")
    corr_mat <- corr_mat[colnames(mat1), colnames(mat2)]
    return(corr_mat)
}



# propr_cross <- function(mat1, mat2,
#                         metric = "rho",
#                         ivar = "clr",
#                         alpha = NA,
#                         p = 100,
#                         mc.cores = 1,
#                         ...) {
#     allcombdf <- dplyr::filter(expand.grid(colnames(mat1), colnames(mat2)), Var1 != Var2)
#     allcomb <- asplit(allcombdf, 1)
#     pair_corr <- parallel::mclapply(allcomb[1],
#                                     function(x) propr::propr(counts = cbind(mat1[,x[1],drop=F], mat2[,x[2],drop=F]),
#                                                              metric = metric,
#                                                              ivar = ivar,
#                                                              alpha = alpha,
#                                                              p = p),
#                                     mc.cores = mc.cores)
#
#     ## matrix only 1 and -1 ???
#     pair_corr2 <- sapply(pair_corr, "[", )
#     corr_df <- purrr::map(pair_corr, brathering::mat_to_df_long) |>
#         dplyr::bind_rows() |>
#         dplyr::distinct(rname, cname, .keep_all = T)
#     corr_mat <- brathering::df_long_to_mat(df = corr_df,
#                                            to_rows = "rname",
#                                            to_cols = "cname",
#                                            values = "value")
#     corr_mat <- corr_mat[colnames(mat1), colnames(mat2)]
#     return(corr_mat)
# }
