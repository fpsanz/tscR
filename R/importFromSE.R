#' Convenient import from a SummarizedExperiment object
#'
#' Import a summarizedExperiment object containing expression data at
#'  different times.
#
#'
#' @param se SummarizedExperiment. Where assays slot can be organized either
#' by samples or by times.
#' @param sample Numeric or character. Sample identifier. See details and
#' vignette
#' @param SE_byTime Logical. Default FALSE. Indicates whether the data is
#'  organized by sample or by time.
#'
#' @details This function enables the integration of an object of the
#' summarizedExpmeriment class and is integrated into all other functions,
#' so there is no need to run it in an isolated way.
#' There are two possible organization options in
#' the slot assays. A) each row is a gene each column,
#' is a sample and each matrix is a time. In this case SE_byTime=FALSE. B)
#' each row is a gene , each column is a time and each
#' matrix is a sample. In this case SE_byTime= TRUE. This concept is
#' illustrated in package vignette.
#'
#' @return Sample data frame where rows are genes and columns are times.,
#'
#'
#' @examples
#'
#' # Each matrix one time / each column one sample (SE_byTime=FALSE)
#' # Code to create dummy data
#'
#' nrows <- 200
#' ncols <- 6
#' time1 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' time2 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' rowRanges <- GenomicRanges::GRanges(rep(c("chr1", "chr2"), c(50, 150)),
#'                      IRanges::IRanges(floor(runif(200, 1e5, 1e6)), width=100),
#'                                           strand=sample(c("+", "-"), 200, TRUE),
#'                                           feature_id=sprintf("ID%03d", 1:200))
#' colData <- S4Vectors::DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                       Samples = LETTERS[1:6],
#'                       row.names=LETTERS[1:6])
#' se <- SummarizedExperiment::SummarizedExperiment(assays=list(time1=time1, time2=time2),
#'                       rowRanges=rowRanges, colData=colData)
#'
#' # Get sample "A" with all times
#'
#' importFromSE(se, sample="A", SE_byTime = FALSE)
#'
#' # or sample = 1 because is first columns in each matrix (each time)
#'
#' # Each matrix one sample / each column one time (SE_byTime=TRUE)
#' # Code to create dummy data
#'
#' nrows <- 200
#' ncols <- 6
#' sampleA <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' sampleB <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' rowRanges <- GenomicRanges::GRanges(rep(c("chr1", "chr2"), c(50, 150)),
#'                      IRanges::IRanges(floor(runif(200, 1e5, 1e6)), width=100),
#'                                           strand=sample(c("+", "-"), 200, TRUE),
#'                                           feature_id=sprintf("ID%03d", 1:200))
#'  colData <- S4Vectors::DataFrame(Time=paste("time",seq(1:6), sep=""),
#'                       sampleA = rep("A",6),
#'                       sampleB = rep("B", 6),
#'                       row.names = paste("time",seq(1:6), sep=""))
#' se <- SummarizedExperiment::SummarizedExperiment(assays=list(sampleA=sampleA, sampleB=sampleB),
#'                       rowRanges=rowRanges, colData=colData)
#' # Get sample "sampleA" with all times
#'
#' importFromSE(se, sample=1, SE_byTime = TRUE)
#'
#' # or sample = 1 because is the first matrix in assays structure.
#'
#' @author  Fernando Pérez-Sanz (\email{fernando.perez8@@um.es})
#' @author  Miriam Riquelme-Pérez (\email{miriam.riquelmep@@gmail.com})

importFromSE <- function(se, sample, SE_byTime = FALSE) {
    if(!is(se, "SummarizedExperiment")){
        stop("Function applicable only to 'SummarizedExperiment' objects")
    }
  if (is.null(sample)) {
    stop("Either sample name or sample number must be provided")
  }
  col = sample
  if (!SE_byTime) {
    tsdf <- as.data.frame(lapply(assays(se), function(se)
      se[, col]))
    names(tsdf) <- paste("time", seq(1,dim(tsdf)[2]), sep = "")
    return(tsdf)
  }
  if (SE_byTime) {
    if (is.numeric(col)) {
      tsdf <- assay(se, i = col)
      return(tsdf)
    } else{
      if (col %in% names(assays(se))) {
        tsdf <- assays(se)[[col]]
        return(tsdf)
      } else {
        stop(paste(
          "Invalid assay/sample name. Posible values ",
          paste(names(assays(se)), collapse = ", ")
        ),
        sep = "")
      }
    }
  }
}
