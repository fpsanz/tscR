context("test data input/output functions")

test_that("Input/Output data are a correct object", {
    data(tscR)
    time <- c(1,2,3)
    out <- slopeDist(tscR, time)
    k <- 3
    cluster <- getClusters(out, k=k)
    expect_equal(class(out), "dist")
    expect_match(class(cluster), "pam|partition")
    expect_equal( length(cluster$medoids), k)
    expect_equal( length(cluster$clustering), dim(tscR)[1])
    })

