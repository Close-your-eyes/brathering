% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_portable_files.R
\name{make_portable_filename}
\alias{make_portable_filename}
\title{Make file names portable}
\usage{
make_portable_filename(x, allow = c("-", "."), repl = "_", urldecode = T)
}
\arguments{
\item{x}{vector of file paths}

\item{allow}{values to allow besides alnum}

\item{repl}{replacement for all not permitted values}

\item{urldecode}{also decode URL encoding}
}
\description{
That means to make them compatible with many systems by replacing
special non-numeric and non-character values.
}
