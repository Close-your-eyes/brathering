% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_leverage.R
\name{sample_leverage}
\alias{sample_leverage}
\title{Sampling with leverage-based probabilities}
\usage{
sample_leverage(
  x,
  size = NULL,
  replace = FALSE,
  leverage = T,
  cluster = NULL,
  seed = 42,
  n = 1,
  ...
)
}
\arguments{
\item{x}{numeric matrix with observations as rows and features as columns}

\item{size}{see base::sample}

\item{replace}{see base::sample}

\item{leverage}{use leverage score as probabilities,
calculated by Seurat::LeverageScore}

\item{cluster}{pre-computed clusters of x, x is not needed when cluster is
provided}

\item{seed}{seed for replication of random results}

\item{n}{number of samples to draw}

\item{...}{arguments to Seurat::LeverageScore}
}
\value{
numeric vector (sampled row indices)
}
\description{
Sample rows from a numeric matrix with leverage scores as probabilities.
This more like avoids that rare communities are lost due to sampling.
}
\examples{
\dontrun{
# create clearly separated communities of different sizes
comm1 <- data.frame(x=rnorm(10000, mean=10), y=rnorm(10000, mean=10), z=rnorm(10000, mean=10))
comm2 <- data.frame(x=rnorm(1000, mean=20), y=rnorm(1000, mean=10), z=rnorm(1000, mean=0))
comm3 <- data.frame(x=rnorm(100, mean=-40), y=rnorm(100, mean=-10), z=rnorm(100, mean=-10))
comm4 <- data.frame(x=rnorm(20, mean=-10), y=rnorm(20, mean=-20), z=rnorm(20, mean=30))
comm_ident <- c(rep(c("A", "B", "C", "D"), sapply(list(comm1,comm2,comm3,comm4), nrow)))
all_comm <- cbind(do.call(rbind, list(comm1,comm2,comm3,comm4)),comm_ident)
# hdos::plotly3dplot(all_comm, color = "comm_ident")

# calc their leverage
levscore <- Seurat::LeverageScore(t(all_comm[,1:3]))
# members of rare communities have higher leverage
mean(levscore[1:10000])
mean(levscore[10001:11000])
mean(levscore[11001:11100])
mean(levscore[11101:11110])

# compare sampling
inds <- sample_leverage(x = as.matrix(all_comm[,1:3]), 100)
table(comm_ident[inds]) # sampled communities are similar in size
inds2 <- inds <- sample_leverage(as.matrix(all_comm[,1:3]), 100, leverage = FALSE)
table(comm_ident[inds2]) # community D is lost and A dominates

# compute cluster before
clst <- fcexpr::get_hclust_clusters(all_comm[,1:3], k = c(4,8,10))
rle(clst[,1]) # matches perfectly, k = 4
rle(clst[,2]) # k = 8
table(clst[,2])
inds3 <- sample_leverage(x = as.matrix(all_comm[,1:3]), 100, cluster = clst[,2])
table(comm_ident[inds3]) # wrong cluster number also yields appropriate results
}
}
