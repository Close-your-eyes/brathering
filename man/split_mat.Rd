% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_mat.R
\name{split_mat}
\alias{split_mat}
\title{Split matrices just like splitting a data frame.}
\usage{
split_mat(x, f, byrow = TRUE, ...)
}
\arguments{
\item{x}{a matrix}

\item{f}{a character or factor vector for splitting}

\item{byrow}{split by rows (T) or by columns (F)}

\item{...}{arguments passed to split}
}
\value{
a list of length unique(f) of splitted matrices in returned
}
\description{
split.data.frame method from base R actually does the job.
The name is very intuitive though and by default it
only splits along rows. For splitting by column two
transposations would be needed which is not very elegant
in case of large matrices. So here you can define if
you want to split the matrix along its rows or columns.
}
\examples{
m <- matrix(rnorm(20), nrow = 4)
split_mat(m, c(1,1,2,2,2), byrow = FALSE)
split_mat(m, c(1,1,2,2), byrow = TRUE)
}
