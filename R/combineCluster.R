# Función combinar 2 clusters
combineCluster <- function(x, y){
  if(!inherits(x,"pam") | !inherits(x, "pam")){
    stop("X and Y must be pam objects")
  }
  uclust <- as.data.frame( cbind( c( 1:length( x$clustering )), x$clustering, y$clustering ) )
  uclust$V4 <- NA
  uclust$V4 <- (uclust$V2*100) + (uclust$V3*10)
  uclust$V4 <- as.numeric(factor(uclust$V4, labels = 1:length(unique(uclust$V4) ) ) )
  result <- list(clustering = uclust$V4)
  class(result) <- c("pam", "partition")
  return(result)
}
