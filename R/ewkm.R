# TODO
#
# Get the random seed stuff synchronised with R's set.seed.
# Calculate sum of squares measures.
#

ewkm <- function(x, k, lambda=1, maxiter=100, delta=0.00001)
{
  if (missing(k)) 
    stop("the number of clusters 'k' must be provided")
  
  vars <- colnames(x)

  nr <- as.integer(nrow(x))
  nc <- as.integer(ncol(x))
  k <- as.integer(k)
  
  Z <- .C("ewkm",
          x=as.double(x),
          nr=nr,
          nc=nc,
          k=k,
          lambda=as.double(lambda),
          maxiter=as.integer(maxiter),
          delta=as.double(delta),
          iterations=integer(1),
          cluster=integer(nr),
          centers=double(k * nc),
          weights=double(k * nc),
          PACKAGE="siatclust")

  centers <- matrix(Z$centers, ncol=ncol(x))
  colnames(centers) <- vars

  weights <- matrix(Z$weights, ncol=ncol(x))
  colnames(weights) <- vars

  cluster <- Z$cluster + 1
  
  size <- aggregate(cluster, list(cluster=cluster), length)[[2]]
  
  result <- list(cluster=cluster,
                 centers=centers,
                 totss=0,  # Should we get this from C
                 withinss=0,  # Should we get this from C
                 tot.withinss=0,  # Should we get this from C
                 size=size, # Should we get this from C
                 iterations=Z$iterations,
                 weights=weights)

  class(result) <- "kmeans"

  return(result)
#  return(Z)
}
