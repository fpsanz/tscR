
slopeDist <- function(x, time){
  if(dim(x)[1]>100){
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
