#' Generic clustering function
#'
#' Compute clustering with pam function and a distance class object.
#'
#'
#' @param x Numeric distance object obtained with dimension n x n.
#' @param k Numeric. Number of clusters
#'
#' @details This function is a wrapper of pam (cluster). x must be a dist object obtained from frechetdist, slopedist
#'  or any other distance metric on condition
#'  that it be an object of the dist class and has dimensions nxn, where n is equal to the number of trajectories.
#'
#' @return Object of class "pam". See \code{\link[cluster]{pam.object}} for details
#'
#'
#' @examples
#'
#' data(tscR)
#' time <- c(1,2,3)
#' dist_tscR <- slopeDist(tscR, time)
#' res.cluster <- getClusters(dist_tscR, 3)
#'
#'
#' @seealso \code{\link[cluster]{pam}, \link{plotClusters}.}
#'
#' @author  Fernando Pérez-Sanz (\code{fernando.perez8@@um.es})
#' @author  Miriam Riquelme Pérez (\code{miriam.riquelmep@@gmail.com})


getClusters <- function(x, k){
  if( !inherits(x, "dist") ){
    stop("X must be a dist class matrix")
  }
  myclust <- pam(x, k=k, diss = T)
  return(myclust)
}
