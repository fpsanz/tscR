# Función para plotear las trajectorias coloreadas según el cluster
plotCluster <- function(data, clust, ncluster){
  if( length(ncluster)==1 && ncluster == "all"){
    colors <- tscR:::fcolor(clust$clustering)
    matplot(t(data), lty = 1, type = "l", col=colors,
            ylab = "Trajectories", xlab="Time" )
  }
  if( length(ncluster)==1 && ncluster != "all"){
    clusteres <- as.numeric( clust$clustering)
    colors <- "red"
    matplot(t(data[clusteres==ncluster,]), lty = 1, type = "l", col=colors,
            ylab = "Trajectories", xlab="Time" )
  }
  if( is.numeric(ncluster) & length(ncluster)>1) {
    n <- ceiling( length( ncluster ) / 2 )
    clusteres <- as.numeric(clust$clustering)
    colors <- tscR:::fcolor(c(1:length(ncluster)))
    par( mfrow = c(n,2) )
    c=1
    for(i in ncluster){
      matplot(t(data[clusteres==i,]), lty = 1, type = "l", col=colors[c],
              ylab="Trajectories",xlab="Time"  )
      c = c + 1
    }
    par( mfrow = c(1,1) )
  }
}

