#' Combine clusters function
#'
#' Combine clusters from two different clustering
#'
#'
#' @param x Object of class pam. Output of getClusters.
#' @param y Object of class pam. Output of getClusters.
#'
#' @details This function combines the clusters obtained in getClusters
#'  with 'slope' distance and the ones obtained with Frechet distance,
#'   resulting in a new clustering combining both distances.
#'
#' @return Object of class 'pam'. See \code{\link[cluster]{pam.object}}
#'  for details
#'
#'
#' @examples
#'
#' data(tscR)
#' data <- tscR
#' time <- c(1,2,3)
#' dist_slope <- slopeDist(data, time)
#' dist_frechet <- frechetDistC(data, time)
#' slope.cluster <- getClusters(dist_slope, 3)
#' frechet.cluster <- getClusters(dist_frechet, 4)
#' comb.cluster <- combineCluster(slope.cluster, frechet.cluster)
#'
#' @seealso \code{\link{getClusters}, \link{plotCluster}}
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})

combineCluster <- function(x, y)
{
    if (!inherits(x, "pam") | !inherits(x, "pam"))
    {
        stop("X and Y must be pam objects")
    }
    uclust <- as.data.frame(cbind(c(seq_len(length(x$clustering))),
        x$clustering, y$clustering))
    uclust$V4 <- NA
    uclust$V4 <- (uclust$V2 * 100) + (uclust$V3 * 10)
    uclust$V4 <- as.numeric(factor(uclust$V4,
                                labels = seq_len(length(unique(uclust$V4)))))
    result <- list(clustering = uclust$V4)
    class(result) <- c("pam", "partition")
    return(result)
}
