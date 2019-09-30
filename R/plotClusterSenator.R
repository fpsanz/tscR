## plot clusterSenators
##TODO acabar  la funcion plotclusterSenator para que plotee los distintos clusters, juntos-separados etc
plotClusterSenator <- function(x, multiple=F, ncluster=1){
  if(class(x)!="imputeSenator"){
    stop("x must be imputeSenator class from imputeSenator function")
  }
  if(multiple==TRUE & ncluster>6){
    stop("ncluster >6 are too many to plot")
  }
  if(multiple==FALSE){
    clusteres <- as.numeric(gsub("end","",x@endCluster))
    colors <- fcolor(clusteres)
    matplot(t(x@data), lty = 1, type = "l", col=colors )
  } else{
    if(length(ncluster)==1 & ncluster != "all"){
      clusteres <- as.numeric( gsub ("end","",x@endCluster) )
      posCluster <- which(clusteres %in% ncluster)
      posCluster2 <- clusteres[posCluster]
      colors <- "red"
      matplot(t(x@data[clusteres==ncluster,]), lty = 1, type = "l", col=colors )
    }else {
      n <- ceiling(length(ncluster)/2)
      clusteres <- as.numeric( gsub ("end","",x@endCluster) )
      posCluster <- which(clusteres %in% ncluster)
      posCluster2 <- clusteres[posCluster]
      colors <- fcolor(c(1:length(ncluster)))
      par( mfrow = c(n,2) )
      c=1
      for(i in ncluster){
        matplot(t(x@data[clusteres==i,]), lty = 1, type = "l", col=colors[c],
                ylab="Trajectories",xlab="Time"  )
        c = c + 1
      }
      par( mfrow = c(1,1) )
    }
  }
}
