#' slopeDist
#'
#' Compute pairswise distance based on slopes in a matrix of trajectories
#'
#'
#' @param x Numeric matrix or data.frame with trajectory values. Rows are trajectories,
#'  columns are time or similar.
#' @param time Numeric vector with time data (time intervals), with equal length to columns number in x.
#'
#' @return A dist class object of size NxN, where N is rows number in the input data
#'
#'
#' @examples
#'
#' data(tscR)
#' time <- c(1,2,3)
#' dist_tscR <- slopeDist(tscR, time)
#'
#'
#' @seealso \code{\link{frechetDistC} and \link{frechetDist} (R and slower versión than frechetDistC.)}
#'
#' @author  Fernando Pérez-Sanz (\code{fernando.perez8@@um.es})
#' @author  Miriam Riquelme Pérez (\code{miriam.riquelmep@@gmail.com})


slopeDist <- function(x, time){
  if(dim(x)[1]>1000){
    warning("Large matrix or data.frame could cause memory problems. You should use imputeSenators function")
  }
  if(is.data.frame(x)){
    x <- as.matrix(x)
  }
  if(!is.matrix(x)){
    stop("X must be matrix or data.frame")
  }
  if(length(time) != dim(x)[2]){
    stop("Time vector must be the same length as x rows")
  }
  N <- dim(x)[1]
  M <- dim(x)[2]
  m1 <- x[,-c(M)]
  m2 <- x[,-c(1)]
  mdif <- m2-m1
  voptimizado <- mdif %*% diag( 1/ (time[2:M] -  time[1:(M-1)]) )
  mydist <- dist(voptimizado, method = "euclidean", diag = T, upper = T)
  return(mydist)
}
