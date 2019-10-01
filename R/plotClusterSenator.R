## plot clusterSenators
plotClusterSenator <- function(x, ncluster=1){
  if(class(x)!="imputeSenator"){
    stop("x must be imputeSenator class from imputeSenator function")
  }
  if( length(ncluster)==1 && ncluster == "all"){
    clusteres <- as.numeric(gsub("end","",x@endCluster))
    colors <- fcolor(clusteres)
    matplot(t(x@data), lty = 1, type = "l", col=colors,
            ylab = "Trajectories", xlab="Time" )
  }
    if( length(ncluster)==1 && ncluster != "all"){
      clusteres <- as.numeric( gsub ("end","",x@endCluster) )
      posCluster <- which(clusteres %in% ncluster)
      posCluster2 <- clusteres[posCluster]
      colors <- "red"
      matplot(t(x@data[clusteres==ncluster,]), lty = 1, type = "l", col=colors,
              ylab = "Trajectories", xlab="Time" )
    }
    if( is.numeric(ncluster) & length(ncluster)>1 ) {
      n <- ceiling( length( ncluster ) / 2 )
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

