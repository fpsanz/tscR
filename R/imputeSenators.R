#' Preclustering function for large data
#'
#' Compute clustering with clara function to obtain a number of 'senators'
#
#'
#' @param x Numeric matrix or data.frame with trajectory values. Rows are
#'  trajectories, columns are time or similar. SummarizedExperiment
#'  object can be provided for compatibility with bioconductor container
#'   (for more information see vignette).
#' @param k Numeric. Number of senators
#' @param ... Other arguments to pass to importFromSE if _x_
#' is SummarizedExperiment-class.
#'
#' @details Calculates a series of senators representing a large set of
#' trajectories that
#'  would otherwise be computationally very expensive. For it, by means of the
#'  \link{clara} function
#'   of the cluster package a clustering is made obtaining the centroids as
#'   senators.
#'    These centroids can then be clustered based on the slope distance or
#'    Frechet or both.
#'     Finally, the data set will be assigned to the same cluster your senator
#'     is assigned to.
#'
#' @return List with three slots:
#'
#' \describe{
#'   \item{data}{Dataframe with original data.}
#'   \item{senatorData}{Matrix with senator trajectories.}
#'   \item{senatorCluster}{Vector with senator clusters.}
#'   }
#'
#' @examples
#'
#' data( tscR )
#' data <- tscR
#' time <- c( 1, 2, 3 )
#' senators <- imputeSenators( data, k = 100 )
#' senatorDist <- slopeDist( senators$senatorData, time )
#' sClust <- getClusters( senatorDist, k = 5 )
#' plotCluster( senators$senatorData, sClust, 2 )
#'
#' @seealso \code{\link{plotClusterSenator}, \link{imputeSenatorToData},
#'  \link{importFromSE}.}
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})

imputeSenators <- function(x, k = 100, ...) {
    if(is(df, "SummarizedExperiment")){
        x <- importFromSE(x, ...)
    }
    if (0.1 * nrow(x) < k) {
        k = 0.1 * nrow(x)
        cat(paste("Setting k to", k, ". 10% of total data"))
    }
    if (k > 100) {
        warning("K higher than 100 has severe computacional cost.
                Setting k to 100.")
        k = 100
    }
    result <- clara(x, k)
    # senatorsWeight <-
    # as.integer(table(result$clustering)) mySenators
    # <- result$clustering
    senatorsData <- result$medoids
    # reOrder <-
    # sort(table(result$clustering),decreasing=TRUE)
    # mySenators <- match(mySenators,names(reOrder))
    senatorsCluster <- seq(1, k)
    names(senatorsCluster) <- paste("sen", seq_len(k),
                                    sep = "")
    # senatorsWide <-
    # senatorsWide[as.integer(names(reOrder)),]
    rownames(senatorsData) <- names(senatorsCluster)
    mySenator <- data.frame(id = x,
                            senators = paste("sen",
                                             result$clustering, sep = ""))
    senatorObj <-
        list(data = mySenator,
             senatorData = senatorsData,
             senatorCluster = senatorsCluster)
    return(senatorObj)
}
