context("test data output functions")

test_that("Output data is a imputeSenator object", {
    data(tscR)
    k=100
    senators <- imputeSenators( tscR, k = 100 )
    sdistSen <- frechetDistC( senators$senatorData, time = c( 1, 2, 3 ) )
    cSenators <- getClusters( sdistSen, k = 4 )
    endCluster <- imputeSenatorToData(senators, cSenators)
    expect_match(class(endCluster), "imputeSenator")
    })
