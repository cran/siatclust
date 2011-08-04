\name{plot.ewkm}
\alias{plot.ewkm}
\title{
  
  Plot Entropy Weighted K-Means Weights

}
\description{

  Plot a heatmap showing the variable weights from the subspace clustering.

}
\usage{

\method{plot}{ewkm}(x, type="lattice", ...)

}
\arguments{

  \item{x}{an object of class ewkm.}

  \item{type}{can be "lattice" or "standard". The "standard" choice uses
    heatmap() to display the weights whilst "lattice" uses levelplot()
    with dendrgramGlobs from the lattice package.}
  
  \item{...}{ignored.}

}
\details{
  
  The entopy weighted k-means clustering algorithm is a subspace
  clusterer ideal for high dimensional data. Along with each cluster we
  also obtain variable weights that provide a relative measure of the
  importance of each variable to that cluster.

  This plot visualises these relative measures of variable importance
  for each of the clusters using a heatmap. The top dendrogram
  highlights the relationship between the clusters and the right side
  dendrogram provides a visual clue to the correlation between the variables.

}
\author{

  Graham J Williams
  
}
\examples{

myewkm <- ewkm(iris[1:4], k=3, lambda=0.5, maxiter=100)

plot(myewkm)

}
