#' Impute clusters from senators to data
#'
#' Assign trajetories from the original data to senator clusters.
#
#'
#' @param senators List object obtained from imputeSenator function
#' @param clusters Pam object obtained from \link{getClusters} or
#'  \link{combineCluster}.
#'
#' @details When it's computed a clustering over senators, it's necessary to
#'  assign those cluster
#' to original data. To do this, it's known which senator each original
#' trajectory belong to, therefore the final cluster of each senator is
#'  identified and the trajectories of that senator are assigned to its
#'   definitive cluster.
#'
#' @return Object of \link{imputeSenator-class}.
#'
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
#' endCluster <- imputeSenatorToData( senators, sClust )
#'
#' @seealso \code{\link{plotClusterSenator}, \link{imputeSenators},
#'  \link{getClusters}, \link{imputeSenator-class}.}
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})

imputeSenatorToData <- function(senators, clusters) {
    if( length(senators)!=3 | !is.list(senators)){
        stop("Senators must be a list provided by imputeSenators")
    }
    if( !is(clusters, "pam")) {
        stop("clusters must be a pam object provided by getClusters")
    }
    data <- senators$data
    data <- data %>% mutate_at(vars(senators), as.character)
    senData <- senators$senatorData
    combinedCluster <- data.frame(
        senId = rownames(senData),
        combClust = paste("end", clusters$clustering,
                          sep = "")
    )
    combinedCluster <- combinedCluster %>% mutate_all(as.character)
    endData <-
        left_join(data, combinedCluster, by = c(senators = "senId"))
    finalData <-
        new(
            "imputeSenator",
            data = as.data.frame(endData[,
                                         seq_len((ncol(endData) - 2))]),
            senators = gsub("sen",
                            "", endData$senators),
            endCluster = gsub("end",
                              "", endData$combClust)
        )
    return(finalData)
}
