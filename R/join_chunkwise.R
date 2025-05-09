#' Join large number of data frames chunkwise
#'
#' It is quicker with a reasonable number of chunk.
#'
#' @param df_list list of data frames
#' @param join_fun function to use for joining; must be one of dplyrs functions
#' @param join_by column(s) to join by; passed to "by" arg of join_fun
#' @param chunks number of chunks
#' @param progress print progress bar?
#' @param rm_invalid remove invalid list elements (e.g, no data frame or
#' no join_by column)
#'
#' @return a single data frame
#' @export
#'
#' @examples
#' # Generate a list of n data frames
#' ndf <- 200
#' nrows <- 10000
#' shared_id <- data.frame(id = 1:nrows)
#' generate_df <- function(i) {
#'     n_extra_cols <- sample(2:5, 1)  # Random number of extra columns (2 to 5)
#'     extra_cols <- replicate(n_extra_cols, rnorm(nrows), simplify = FALSE)
#'     names(extra_cols) <- paste0("var", i, "_", seq_len(n_extra_cols))
#'     df <- data.frame(id = shared_id$id, extra_cols)
#'     return(df)
#' }
#' df_list <- lapply(1:ndf, generate_df)
#'
#' # vary chunk size
#' benchres <- bench::mark(join_chunkwise(df_list = df_list, join_by = "id", chunks = 1),
#'                         join_chunkwise(df_list = df_list, join_by = "id", chunks = length(df_list)/5),
#'                         join_chunkwise(df_list = df_list, join_by = "id", chunks = length(df_list)/10),
#'                         join_chunkwise(df_list = df_list, join_by = "id", chunks = length(df_list)/20),
#'                         join_chunkwise(df_list = df_list, join_by = "id", chunks = length(df_list)/40))

join_chunkwise <- function(df_list,
                           join_fun = dplyr::full_join,
                           join_by,
                           chunks = length(df_list)/10,
                           progress = F,
                           rm_invalid = F) {


    stopifnot("join_by is missing" = !missing(join_by),
              "df_list must be a list." = is.list(df_list),
              "chunks must be numeric." = is.numeric(chunks),
              "chunks must be length one." = length(chunks) == 1,
              "chunks must be greater than zero." = chunks > 0)

    if (!all(inds <- purrr::map_lgl(df_list, is.data.frame))) {
        if (rm_invalid) {
            df_list <- df_list[inds]
        } else {
            stop("df_list contains non data frame objects.")
        }
    }
    if (!all(inds <- purrr::map_lgl(df_list, ~join_by %in% names(.x)))) {
        if (rm_invalid) {
            df_list <- df_list[inds]
        } else {
            stop("join_by is not found in every df_list.")
        }
    }

    join_fun <- match.fun(join_fun)

    #chunks <- min(chunks, length(df_list))
    if (chunks > length(df_list)/2) {
        message("chunks is set to length(df_list)/2 to have at least two elements per chunk.")
        chunks <- length(df_list)/2
    }
    if (chunks > 1) {
        splits <- length(df_list)/chunks
        df_list <- split(df_list, ceiling(seq_along(df_list)/splits))
        df_list <- purrr::map(
            df_list,
            purrr::reduce,
            join_fun,
            by = join_by,
            .progress = progress
        )
    }
    # final join
    df_list <- purrr::reduce(df_list, join_fun, by = join_by)

    invisible(df_list)
}

