#' Dummy trajectories data
#'
#' A dataset containing 300 tajectories and 3 time points
#'
#' @details This dataset has been created specifically to be able to illustrate the
#' operation of the package with different distance metrics. Thus, from 3-4 hand-created
#' trajectories (ascending, descending, quasi-horizontal) we have generated 300 
#' trajectories with random variations from the original ones. The code used was similar 
#' to the one attached here:
#' 
#' @examples 
#' 
#' df <- data.frame(T1 = c(4,3.9,4.1,4), 
#'                  T2=c(5.5, 4.3, 3.7, 2.5),
#'                  T3 = c(7, 3.9,4.1, 1))
#' df1 <- matrix(NA, nrow=100, ncol=3)
#' df2 <- matrix(NA, nrow=100, ncol=3)
#' df3 <- matrix(NA, nrow=100, ncol=3)
#' df4 <- matrix(NA, nrow=100, ncol=3)
#' for(i in seq(1,75)){
#'  df1[i,] <- jitter(as.numeric(df[1,]), factor = 2.5)
#'  df2[i,] <- jitter(as.numeric(df[2,]), factor = 7.5)
#'  df3[i,] <- jitter(as.numeric(df[3,]), factor = 7.5)
#'  df4[i,] <- jitter(as.numeric(df[4,]), factor = 2.5)
#' }
#' df <- as.data.frame(rbind(df1,df2,df3, df4))
#' names(df) <- c("T1","T2","T3")
#' 
#' @format A data frame 300 rows and 3 columns:
#' \describe{
#'   \item{T1}{time interval}
#'   \item{T2}{time interval}
#'   \item{T3}{time interval}
#' }
#' 
#' @source Simulated data
"tscR"
