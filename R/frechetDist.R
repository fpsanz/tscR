#' Pairwise Frechet distance
#'
#' Compute pair-wise Frechet distance in a matrix of trajectories
#'
#'
#' @param x Numeric matrix or data.frame with trajectory values.
#'  Rows are trajectories, columns are time or similar.
#'
#' @param time Numeric vector with time data (time intervals),
#'  with equal length to columns number in x.
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
#'   (C and faster versión than frechetDist).}
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})


frechetDist <- function(x, time) {
  warning("This function is slower than frechetDistC.")
  warning("You should use C version")
  if (dim(x)[1] > 1000) {
    warning(
      "Large matrix or data.frame could cause memory
        problems. You should use imputeSenators function"
    )
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (length(time) != dim(x)[2]) {
    stop("Time vector must be the same length as x columns")
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
