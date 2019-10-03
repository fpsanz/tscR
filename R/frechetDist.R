#' frechetDist
#'
#' Compute pairswise Frechet distance in a matrix of trajectories
#'
#'
#' @param x Numeric matrix or data.frame with trajectory values. Rows are trajectories,
#'  columns are time or similar.
#' @param time Numeric vector with time data (time intervals), with equal length to columns number in x.
#'
#' @details This function is a R adaptation of the distFrechet code from kmlShape package for use with a matrix or
#' a data.frame so that the user can compute pairwise distances between all trajectories.
#'
#' @return A dist class object of size NxN, where N is rows number in the input data
#'
#'
#' @examples
#'
#' data(tscR)
#' time <- c(1,2,3)
#' dist_tscR <- frechetDist(tscR, time)
#'
#'
#' @seealso \code{distFrechet (package kmlShape), \link{slopeDist}, \link{frechetDist} (C andf faster versión than frechetDist).}
#'
#' @author  Fernando Pérez-Sanz (\code{fernando.perez8@@um.es})
#' @author  Miriam Riquelme Pérez (\code{miriam.riquelmep@@gmail.com})


frechetDist <- function(x, time){
  warning("This function is slower than frechetDistC.")
  warning("You should use C version")
  if(dim(x)[1]>1000){
    warning("Large matrix or data.frame could cause memory problems. You should use imputeSenators function")
  }
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
