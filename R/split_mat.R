#' Split matrices just like splitting a data frame.
#'
#' split.data.frame method from base R actually does the job.
#' The name is very intuitive though and by default it
#' only splits along rows. For splitting by column two
#' transposations would be needed which is not very elegant
#' in case of large matrices. So here you can define if
#' you want to split the matrix along its rows or columns.
#'
#' @param x a matrix
#' @param f a character or factor vector for splitting
#' @param byrow split by rows (T) or by columns (F)
#' @param ... arguments passed to split
#'
#' @return a list of length unique(f) of splitted matrices in returned
#' @export
#'
#' @examples
#' m <- matrix(rnorm(20), nrow = 4)
#' split_mat(m, c(1,1,2,2,2), byrow = FALSE)
#' split_mat(m, c(1,1,2,2), byrow = TRUE)
split_mat <- function(x, f, byrow = TRUE, ...) {


  ## https://stackoverflow.com/questions/62161916/is-there-a-function-in-r-that-splits-a-matrix-along-a-margin-using-a-factor-or-c
  ## modified from base function split.data.frame (to avoid 2 x transposation)
  ## multi dirs: split count matrix by orig.idents

  if (!is.logical(byrow)) {
    stop("byrow should be logical, TRUE or FALSE.")
  }

  if (!is.matrix(x) && !methods::is(x, "sparseMatrix")) {
    stop("x should be a (sparse) matrix.")
  }

  if (byrow) {
    if (length(f) != nrow(x)) {
      stop("length(f) should be equal to nrow(x).")
    }
    lapply(split(x = seq_len(nrow(x)), f = f, ...), function(ind) x[ind,,drop = FALSE])
  } else {
    if (length(f) != ncol(x)) {
      stop("length(f) should be equal to ncol(x).")
    }
    lapply(split(x = seq_len(ncol(x)), f = f, ...), function(ind) x[,ind,drop = FALSE])
  }

}
