% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceiling_floor2.R
\name{floor2}
\alias{floor2}
\title{Floor to a given decimal place}
\usage{
floor2(x, accuracy = 0.1, fun = base::floor)
}
\arguments{
\item{x}{numeric vector}

\item{accuracy}{decimal accuracy}

\item{fun}{function}
}
\value{
numeric vector
}
\description{
Floor to a given decimal place
}
\examples{
floor2(rnorm(20))
}
