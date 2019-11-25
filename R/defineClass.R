##' Class 'inputeSenator'
##' This class represents the result imputeSenators function
##'
##'
##' @name imputeSenator-class
##' @aliases imputeSenator-class show,imputeSenator-method
##' @docType class
##' @slot data Dataframe with original data
##' @slot senators Senators data value
##' @slot endCluster Final cluster assignment
##' @exportClass imputeSenator
##' @author  Fernando Pérez Sanz (\email{fernando.perez8@@um.es})
##' @author  Miriam Riquelme Pérez (\email{miriam.riquelmep@@gmail.com})
##' @seealso \code{\link{imputeSenators}}
##' @keywords classes
setClass(
  "imputeSenator",
  slots = list(
    data = "data.frame",
    senators = "character",
    endCluster = "character"
  )
)
