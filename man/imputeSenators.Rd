% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputeSenators.R
\name{imputeSenators}
\alias{imputeSenators}
\title{Preclustering function for large data}
\usage{
imputeSenators(x, k = 100, ...)
}
\arguments{
\item{x}{Numeric matrix or data.frame with trajectory values. Rows are
trajectories, columns are time or similar. SummarizedExperiment
object can be provided for compatibility with bioconductor container
 (for more information see vignette).}

\item{k}{Numeric. Number of senators}

\item{...}{Other arguments to pass to importFromSE if _x_
is SummarizedExperiment-class.}
}
\value{
List with three slots:

\describe{
  \item{data}{Dataframe with original data.}
  \item{senatorData}{Matrix with senator trajectories.}
  \item{senatorCluster}{Vector with senator clusters.}
  }
}
\description{
Compute clustering with clara function to obtain a number of 'senators'
}
\details{
Calculates a series of senators representing a large set of
trajectories that
 would otherwise be computationally very expensive. For it, by means of the
 \link{clara} function
  of the cluster package a clustering is made obtaining the centroids as
  senators.
   These centroids can then be clustered based on the slope distance or
   Frechet or both.
    Finally, the data set will be assigned to the same cluster your senator
    is assigned to.
}
\examples{

data( tscR )
data <- tscR
time <- c( 1, 2, 3 )
senators <- imputeSenators( data, k = 100 )
senatorDist <- slopeDist( senators$senatorData, time )
sClust <- getClusters( senatorDist, k = 5 )
plotCluster( senators$senatorData, sClust, 2 )

}
\seealso{
\code{\link{plotClusterSenator}, \link{imputeSenatorToData},
 \link{importFromSE}.}
}
\author{
Fernando Pérez-Sanz (\email{fernando.perez8@um.es})

Miriam Riquelme-Pérez (\email{miriam.riquelmep@gmail.com})
}
