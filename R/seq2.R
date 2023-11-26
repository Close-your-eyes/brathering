
seq2 <- function(from = 1, to = 1) {
  x <- seq2_default(from = from, to = to)

  # make sure always a list is returned
  if (is.matrix(x)) {
    x <- unname(as.list(as.data.frame(x)))
  }
  return(x)
}

seq2_default <- Vectorize(seq.default, vectorize.args = c("from", "to"))

