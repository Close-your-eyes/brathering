% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot2.R
\name{plot2}
\alias{plot2}
\title{Plot data quickly with scattermore}
\usage{
plot2(
  x,
  color = NULL,
  palette = NULL,
  colortype = c("d", "c", "discrete", "continuous"),
  legend = T,
  ...
)
}
\arguments{
\item{x}{matrix or data frame, col1 becomes x, col2 becomes y}

\item{color}{name of column for color or .density_r or .density_s for
coloring according to point density}

\item{palette}{color palette}

\item{colortype}{type of color scale}

\item{legend}{plot legend?}

\item{...}{arguments to scattermore::scattermoreplot}
}
\value{
nothing, scattermore plot is plotted
}
\description{
A convenient wrapper to have easy color scale and legend.
}
\examples{
brathering::plot2(brathering::points_2d_circ(100))
}
