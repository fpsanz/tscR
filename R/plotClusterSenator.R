#' Plot trajectories based on clustering
#'
#' Draw trajectories and are colored based on their clusters with imputesenator object
#'
#' @param x imputesenator class object from \link{imputeSenatorToData} function.
#' @param ncluster When nclust = "all", plots all trajectories and cluster together in a single plot.
#' If it's an integer, it draws only trajectories that belong to that cluster. Finally, if
#' it is a numeric vector, it draws trajectories corresponding to each cluster within a subplot.
#'
#' @details It draws trajectories where x axis is time data and y axis trayectory values.
#'
#' @examples
#'
#' data( tscR )
#' data <- tscR
#' time <- c( 1, 2, 3 )
#' senators <- imputeSenators( data, k = 100 )
#' senatorDist <- slopeDist( senators$senatorData, time )
#' sClust <- getClusters( senatorDist, k = 5 )
#' endCluster <- imputeSenatorToData( senators, sClust )
#' plotClusterSenator( endCluster, "all" )
#'


plotClusterSenator <- function(x, ncluster){
  if(class(x)!="imputeSenator"){
    stop("x must be imputeSenator class from imputeSenator function")
  }
  if( length(ncluster)==1 && ncluster == "all"){
    clusteres <- as.numeric(gsub("end","",x@endCluster))
    colors <- tscR:::fcolor(clusteres)
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
      colors <- tscR:::fcolor(c(1:length(ncluster)))
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

