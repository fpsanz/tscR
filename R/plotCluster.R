#' Plot trajectories based on clustering
#'
#' Draw trajectories and are colored based on their clusters
#'
#' @param data Numeric data frame or matrix with de original data.
#' @param clust Object of class pam or partition obtained from getClusters output.
#' @param ncluster When nclust = 'all', plots all trajectories and cluster together in a single plot.
#' If it's an integer, it draws only trajectories that belong to that cluster. Finally, if
#' it is a numeric vector, it draws trajectories corresponding to each cluster within a subplot.
#'
#' @return Plot clustered trayectories
#'
#' @details It draws trajectories where x axis is time data and y axis trayectory values.
#'
#' @examples
#'
#' data(tscR)
#' data <- tscR
#' time <- c(1,2,3)
#' fdist <- frechetDistC(data, time)
#' fclust <- getClusters(fdist, 3)
#' plotCluster(data, fclust, 'all')
#'
#'
#' @seealso \code{\link[graphics]{matplot}, \link{plotClusterSenator}}.
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})


plotCluster <- function(data, clust, ncluster) {
    if (length(ncluster) == 1 && ncluster == "all") {
        colors <- tscR:::fcolor(clust$clustering)
        matplot(
            t(data),
            lty = 1,
            type = "l",
            col = colors,
            ylab = "Trajectories",
            xlab = "Time"
        )
    }
    if (length(ncluster) == 1 && ncluster != "all") {
        clusteres <- as.numeric(clust$clustering)
        colors <- "red"
        matplot(
            t(data[clusteres == ncluster,]),
            lty = 1,
            type = "l",
            col = colors,
            ylab = "Trajectories",
            xlab = "Time"
        )
    }
    if (is.numeric(ncluster) & length(ncluster) > 1) {
        n <- ceiling(length(ncluster) / 2)
        clusteres <- as.numeric(clust$clustering)
        colors <- tscR:::fcolor(seq_len(length(ncluster)))
        par(mfrow = c(n, 2))
        c = 1
        for (i in ncluster) {
            matplot(
                t(data[clusteres == i,]),
                lty = 1,
                type = "l",
                col = colors[c],
                ylab = "Trajectories",
                xlab = "Time"
            )
            c = c + 1
        }
        par(mfrow = c(1, 1))
    }
}
