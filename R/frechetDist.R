frechetDist <- function(x, time){
  require(kmlShape, quietly = T)
  if(!is.matrix(x)){
    x <- as.matrix(x)
  }
  if(length(time) != dim(x)[2]){
    stop("Time vector must be the same length as x columns")
  }
  dfrechet <- matrix(nrow = nrow(x), ncol=nrow(x))
  for(i in seq(1,nrow(x))){
    for(j in seq(1:nrow(x))){
      dfrechet[i,j] <- kmlShape::distFrechet( time , x[i,], time, x[j,], timeScale = 1, FrechetSumOrMax = "max" ) # es la más eficiente
    }
  }
  return(stats::as.dist(dfrechet, diag=T, upper=T))
}
