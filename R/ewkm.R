# TODO
#
# Calculate sum of squares measures.
#

ewkm <- function(x, k, lambda=1, maxiter=100, delta=0.00001, maxrestart=10)
{
  if (missing(k)) 
    stop("the number of clusters 'k' must be provided")
  
  vars <- colnames(x)

  nr <- as.integer(nrow(x))
  nc <- as.integer(ncol(x))
  k <- as.integer(k)
  
  Z <- .C("ewkm",
          x=as.double(as.matrix(x)), # siatclust needs to accept a data.frame
          nr=nr,
          nc=nc,
          k=k,
          lambda=as.double(lambda),
          maxiter=as.integer(maxiter),
          delta=as.double(delta),
          maxrestart=as.integer(maxrestart),
          iterations=integer(1),
          cluster=integer(nr),
          centers=double(k * nc),
          weights=double(k * nc),
          restarts=integer(1),
          totiters=integer(1),
          PACKAGE="siatclust")

  centers <- matrix(Z$centers, ncol=ncol(x))
  colnames(centers) <- vars

  weights <- matrix(Z$weights, ncol=ncol(x))
  colnames(weights) <- vars

  # Identify missing clusters to be removed.  110804 Deal with the
  # case whereby a single cluster is returned. Previous version did it
  # properly in that centers[-ignore,] returns a matrix. But now it
  # returns a vector. So need to use drop=FALSE
  
  ignore <- which(rowSums(centers==0) == ncol(centers))
  if (length(ignore))
  {
    centers <- centers[-ignore,, drop=FALSE]
    weights <- weights[-ignore,, drop=FALSE]
  }

  # Give the rows names.

  rownames(centers) <- 1:nrow(centers)
  rownames(weights) <- 1:nrow(weights)

  cluster <- Z$cluster + 1
  
  size <- aggregate(cluster, list(cluster=cluster), length)[[2]]
  
  result <- list(cluster=cluster,
                 centers=centers,
                 totss=0,  # Should we get this from C
                 withinss=0,  # Should we get this from C
                 tot.withinss=0,  # Should we get this from C
                 size=size, # Should we get this from C
                 iterations=Z$iterations,
                 total.iterations=Z$totiters,
                 restarts=Z$restarts,
                 weights=weights)

  class(result) <- c("kmeans", "ewkm")

  return(result)
#  return(Z)
}

plot.ewkm <- function(x, type="lattice", ...)
{
  if (type == "standard")
    plot.ewkm.standard(x)
  else
    plot.ewkm.lattice(x)
}


plot.ewkm.standard <- function(x)
{
  x  <- t(x$weights)
  rc <- rainbow(nrow(x), start=0, end=.3)
  cc <- rainbow(ncol(x), start=0, end=.3)
  hv <- heatmap(x, col = cm.colors(256), scale="column",
                RowSideColors = rc, ColSideColors = cc, margin=c(5,10),
                xlab = "Cluster")
}

plot.ewkm.lattice <- function(x)
{
  require(lattice)
  require(latticeExtra)
 
  x  <- x$weights
  dd.row <- as.dendrogram(hclust(dist(x)))
  row.ord <- order.dendrogram(dd.row)
 
  dd.col <- as.dendrogram(hclust(dist(t(x))))
  col.ord <- order.dendrogram(dd.col)

  my.colors <- function(n) rev(heat.colors(n))
  
  levelplot(x[row.ord, col.ord],
            aspect = "fill", pretty=TRUE,
            xlab="Cluster", ylab="",
            colorkey = list(space = "left", col=my.colors),
            col.regions=my.colors,
            legend =
            list(right =
                 list(fun = dendrogramGrob,
                      args =
                      list(x = dd.col, ord = col.ord,
                           side = "right",
                           size = 10)),
                 top =
                 list(fun = dendrogramGrob,
                      args =
                      list(x = dd.row, 
                           side = "top"))))
}

