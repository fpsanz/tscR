context("test data output functions")

test_that("Output data is a list with three elements", {
    data(tscR)
    k=100
    out <- imputeSenators(tscR, k=k)
    expect_equal(class(out), "list")
    expect_equal(length(out), 3)
    expect_equal( dim(out$data), c( dim(tscR)[1], dim(tscR)[2]+1) )
    expect_equal( dim(out$senatorData)[1], length(out$senatorCluster))
    })


