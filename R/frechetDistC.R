frechetDistC <- function(x, time){
  if(!is.matrix(x)){
    x <- as.matrix(x)
  }
  if(length(time) != dim(x)[2]){
    stop("Time vector must be the same length as x columns")
  }
  Px <- Qx <- time
  Df <- as.vector(as.matrix(t(x)))
  lenDf <- length(Df)
  resultado <- .C("myCalcMatrixEuclidCumul", Py=numeric(length(Px)), Qy=numeric(length(Qx)), Df=as.numeric(Df),
                  Px=as.numeric(Px), Qx=as.numeric(Qx), tailleP=as.integer(length(Px)), tailleQ=as.integer(length(Qx)),
                  matDistEuclidCumul=numeric(length(Px)*length(Qx)), matDist=numeric((lenDf/length(Px))^2),
                  lenDf=as.integer(length(lenDf)), sumOrMax=as.integer("max"=="sum"), toploop=as.integer(lenDf/length(Px)),
                  PACKAGE = "tscR" )
  res <- matrix( resultado$matDist, nrow=lenDf/length(Px), ncol=lenDf/length(Px), byrow = T)
  return(stats::as.dist(res, diag = T, upper = T))
}
