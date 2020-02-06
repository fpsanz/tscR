context("test data output functions")

test_that("Output data is a square matrix distance", {
    data(tscR)
    time <- c(1,2,3)
    out <- frechetDist(tscR, time)
    expect_equal(class(out), "dist")
    expect_equal(dim(out)[1], dim(out)[2])
    })

