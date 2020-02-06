context("test data input functions")

test_that("Input data is a summarizedExperiment object", {
    nrows <- 200
    ncols <- 6
    time1 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
    time2 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
    rowRanges <- GenomicRanges::GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                     IRanges::IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                                          strand=sample(c("+", "-"), 200, TRUE),
                                          feature_id=sprintf("ID%03d", 1:200))
    colData <- S4Vectors::DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                      Samples = LETTERS[1:6],
                      row.names=LETTERS[1:6])
    se <- SummarizedExperiment::SummarizedExperiment(assays=list(time1=time1, time2=time2),
                      rowRanges=rowRanges, colData=colData)
    expect_error( importFromSE(c(1,2,3), sample = "A"),
                  "Function applicable only to 'SummarizedExperiment' objects")
    impSE <- importFromSE(se, sample=1, SE_byTime = TRUE)
    expect_match(class(impSE), "data.frame|matrix")
})
