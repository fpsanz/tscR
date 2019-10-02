# Función para plotear las trajectorias coloreadas según el cluster
plotCluster <- function(data, clust, ncluster){

  require(graphics)
  matplot(t(data), col = fcolor(clust$clustering), type = "l", lty = 1, add=F)
}

