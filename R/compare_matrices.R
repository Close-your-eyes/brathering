#' Find matching rows/cols of matrices by hashing
#'
#' @param x1 matrix1
#' @param x2 matrix2
#' @param margin 1 = rows, 2 = columns
#' @param match_names filter and order by intersection of x1, x2 (in opposite of
#' margin)
#' @param chunk_size split wise calculation to reduce memory footprint (with
#' sparse matrices)
#'
#' @returns data frame
#' @export
#'
#' @examples
#' x1 <- matrix(as.integer(rnorm(100)), ncol = 20)
#' x2 <- matrix(as.integer(rnorm(100)), ncol = 20)
#' df <- compare_matrices(x1,x2)
#' A <- matrix(rnorm(15), nrow = 5, ncol = 3)
#' rownames(A) <- paste0("row", 1:5)
#' colnames(A) <- paste0("A_col", 1:3)
#'
#' # Second matrix: 5 rows, 4 columns
#' B <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' rownames(B) <- paste0("row", c(3:7))  # overlaps partially: row3, row4, row5
#' colnames(B) <- paste0("B_col", 1:4)
#' compare_matrices(A,B, match_names = T)
compare_matrices <- function(x1,
                             x2,
                             margin = 2,
                             match_names = F,
                             chunk_size = 500,
                             ncores = 0) {

    if (margin == 1) {
        fun <- colnames
        fun2 <- ncol
        fun3 <- nrow
    } else if (margin == 2) {
        fun <- rownames
        fun2 <- nrow
        fun3 <- ncol
    }

    # only one with names
    if (sum(is.null(fun(x1)), is.null(fun(x2))) == 1) {
        message("only one matrix has names.")
    }
    # unequal row/col number
    if (fun2(x1) != fun2(x2) &&
        (sum(is.null(fun(x1)), is.null(fun(x2))) < 2 && !match_names)) {
        stop("unequal number of rows or columns. need names & match_names.")
    }

    if (sum(is.null(fun(x1)), is.null(fun(x2))) < 2 && match_names) {
        stop("at least one col/rowname missing, cannot use match_names.")
    }

    if (match_names) {
        int <- intersect(fun(x1), fun(x2))
        if (margin == 1) {
            x1 <- x1[int,]
            x2 <- x2[int,]
        } else if (margin == 2) {
            x1 <- x1[,int]
            x2 <- x2[,int]
        }
    }

    #mirai::daemons(ncores)
    ## do chunkwise
    n <- fun3(x1) # nrow or ncol
    if (is.null(n) || n == 0) return(character(0))

    # chunkwise to reduce memory footprint
    idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
    if (margin == 1) {

        h1 <- unlist(purrr::map(idx, ~hash_matrix(x1[.x,], margin = margin)))
        h2 <- unlist(purrr::map(idx, ~hash_matrix(x2[.x,], margin = margin)))
    } else if (margin == 2) {

        h1 <- unlist(purrr::map(idx, ~hash_matrix(x1[,.x], margin = margin)))
        h2 <- unlist(purrr::map(idx, ~hash_matrix(x2[,.x], margin = margin)))
        #h1 <- unlist(purrr::map(purrr::in_parallel(\(y) x1[idx], x1 = x1, margin = margin)))
        #h2 <- unlist(purrr::map(purrr::in_parallel(idx, ~hash_matrix(x2[,.x], margin = margin))))
    }

    #mirai::daemons(0)

    if (anyDuplicated(h1)) {
        message("duplicates in x1.")
    }
    if (anyDuplicated(h2)) {
        message("duplicates in x2.")
    }
    int <- length(intersect(unique(h1), unique(h2)))
    message("number of intersecting indices in x1 and x2: ", int)
    df1 <- data.frame(hash = h1) |> dplyr::mutate(ind1 = dplyr::row_number())
    df2 <- data.frame(hash = h2) |> dplyr::mutate(ind2 = dplyr::row_number())
    df <- dplyr::full_join(df1, df2, by = "hash")
    return(df)
}

hash_matrix <- function(x, margin = 1) {
    apply(x, MARGIN = margin, function(y) digest::digest(y, algo = "xxhash64"))
}
