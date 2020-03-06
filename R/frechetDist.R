#' Pairwise Frechet distance
#'
#' Compute pair-wise Frechet distance in a matrix of trajectories
#'
#'
#' @param x Numeric matrix or data.frame with trajectory values.
#'  Rows are trajectories, columns are time or similar. SummarizedExperiment
#'  object can be provided for compatibility with bioconductor container (for
#'  more information see vignette).
#' @param time Numeric vector with time data (time intervals),
#'  with equal length to columns number in x.
#' @param ... Other arguments to pass to importFromSE if _x_
#' is SummarizedExperiment-class.
#'
#' @details This function is a wrapper of the distFrechet code from
#'  kmlShape package for use with a matrix or
#' a data.frame so that the user can compute
#'  pairwise distances between all trajectories.
#'
#' @return A dist class object of size NxN, where N is rows number
#'  in the input data
#'
#'
#' @examples
#'
#' data(tscR)
#' data <- tscR
#' time <- c(1,2,3)
#' dist_tscR <- frechetDist(data, time)
#'
#'
#' @seealso \code{\link[kmlShape]{distFrechet} (package kmlShape),
#'  \link{slopeDist}, \link{frechetDistC}
#'   (C and faster versión than frechetDist), \link{importFromSE}.}
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})


frechetDist <- function(x, time, ...) {
  warning("This function is slower than frechetDistC.\nYou should use C version.")
  if(is(x, "SummarizedExperiment")){
        x <- importFromSE(x, ...)
    }
  if (dim(x)[1] > 1000) {
    warning(
      "Large matrix or data.frame could cause memory problems.
You should use imputeSenators function"
    )
  }
  if (!is.matrix(x)) {
    tryCatch(
        { xx <- as.matrix(x) },
        error = function(e){
            stop( "x must be a matrix or a matrix coercionable object" ) } )
    if(dim(x)[1]!=dim(xx)[1] | dim(x)[2]!=dim(xx)[2]){
        stop("x has been coerced into matrix but its dimensions are not correct")
    }
    x <- xx
  }
  if (!is.vector(time) | !is.numeric(time) | length(time) != dim(x)[2]) {
    stop("Time vector must be a numeric vector with same length as x columns")
  }
  dfrechet <- matrix(nrow = nrow(x), ncol = nrow(x))
  for (i in seq(1, nrow(x))) {
    for (j in seq(1, nrow(x))) {
      dfrechet[i, j] <- kmlShape::distFrechet(time,
                                              x[i,],
                                              time,
                                              x[j,],
                                              timeScale = 1,
                                              FrechetSumOrMax = "max")
    }
  }
  return(stats::as.dist(dfrechet, diag = TRUE, upper = TRUE))
}
