\name{ewkm}
\alias{ewkm}
\title{
  
  Entropy Weighted K-Means

}
\description{

  Perform an entropy weighted subspace k-means.

}
\usage{

 ewkm(x, k, lambda=1, maxiter=100, delta=0.00001)

}
\arguments{

  \item{x}{numeric matrix of observations and variables.}

  \item{k}{target number of clusters.}

  \item{lambda}{parameter for variable weight distribution.}

  \item{maxiter}{maximum number of iterations.}
    
  \item{delta}{maximum change allowed between iterations for convergence.}

}
\details{
  
  The entopy weighted k-means clustering algorithm is a subspace
  clusterer ideal for high dimensional data. Along with each cluster we
  also obtain variable weights that provide a relative measure of the
  importance of each variable to that cluster.

  The algorithm is based on the k-means approach to clustering. An
  initial set of k means are identified as the starting
  centroids. Observartions are clustered to the nearest centroid
  according to a distance measure. This defines the initial
  clustrering. New centroids are then identified based on these
  clusters.
  
  Weights are then calculated for each variable within each cluster,
  based on the current clustering.  The weights are a measure of the
  relative importance of each variable with regard to the membership of
  the observations to that cluster. These weights are then incorporated
  into the distance function, typically reducing the distance for the
  more important variables.

  New centroids are then calculated, and using the weighted distance
  measure each observation is once again clustered to its nearest
  centroid.

  The process continues until convergence (using a measure of dispersion
  and stopping when the change becomes less than delta) or until a
  specified number of iterations has been reached (maxiter).

  Large lambda (e,g, > 3) lead to a relatively even distribution of
  weights across the variables. Small lambda (e.g., < 1) lead to a more
  uneven distribution of weights, giving more discrimintation between
  features. Recommended values are between 1 and 3.
  
}
\value{
  
  Returns an object of class "kmeans", compatible with other functions
  that work with kmeans objects, such as the 'print' method. The object
  is a list with the following components in addition to the components
  of the kmeans object:

  weights: A matrix of weights recording the relative importance of each
  variable for each cluster.

  iterations: The number of iterations before termination.
}
\references{

  Jing Liping, M. K. Ng, J. Z. Huang, An Entropy Weighting k-Means
  Algorithm for Subspace Clustering of High-Dimensional Sparse Data,
  IEEE Transactions on Knowledge and Data Engineering, 19 (8), Aug 2007
  pp 1026--1041.
  
}
\author{

  Qiang Wang, Xiaojun Chen, Graham J Williams, Joshua Z Huang
  
}
\examples{

x <- as.matrix(iris[1:4])
myewkm <- ewkm(x, k=3, lambda=0.5, maxiter=100)

plot(as.data.frame(x), col=myewkm$cluster)

# For comparative testing

mykm <- kmeans(x, 3)

}
