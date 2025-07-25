% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_lims.R
\name{gg_lims}
\alias{gg_lims}
\title{Get axis limits of ggplot object}
\usage{
gg_lims(ggobj)
}
\arguments{
\item{ggobj}{ggplot2 object}
}
\value{
list of numeric vectors
}
\description{
Get axis limits of ggplot object
}
\examples{
ggobj <- ggplot2::ggplot(data = data.frame(xx = rnorm(100, 20,2),
                                           yy = rnorm(100,10,10)),
                         ggplot2::aes(xx,yy)) +
         ggplot2::geom_point()
gg_lims(ggobj)
}
