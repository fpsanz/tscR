#' Pairwise Frechet distance (C version)
#'
#' Compute pairwise Frechet distance in a matrix of trajectories. This function is a C implementation and
#' a lot faster version than frechetDist
#'
#' @param x Numeric matrix or data.frame with trajectory values. Rows are trajectories,
#'  columns are time or similar.
#' @param time Numeric vector with time data (time intervals), with equal length to columns number in x.
#'
#' @details This function is a C adaptation of the distFrechet code from kmlShape package for use with a matrix or
#' a dataframe so that the user can compute pairwise distances between all trajectories.
#'
#' It is highly recommended to use this function over frechetDist because it is a lot faster.
#'
#' @return A dist class object of size NxN, where N is rows number in the input data
#'
#'
#' @examples
#'
#' data(tscR)
#' data <- tscR
#' time <- c(1,2,3)
#' dist_tscR <- frechetDistC(data, time)
#'
#'
#' @seealso \code{\link[kmlShape]{distFrechet} (package kmlShape), \link{slopeDist}, \link{frechetDist} (R and slower versión than frechetDistC).}
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})


frechetDistC <- function(x, time){
  if(dim(x)[1]>1000){
    warning("Large matrix or data.frame could cause memory problems. You should use imputeSenators function")
  }
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
