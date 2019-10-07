##' Class "inputeSenator"
##' This class represents the result imputeSenators function
##'
##'
##' @name imputeSenator-class
##' @aliases imputeSenator-class show,imputeSenator-method
##' @docType class
##' @slot data Impute senator result
##' @slot senators Senators data value
##' @slot endCluster Final cluster assignment
##' @exportClass imputeSenator
##' @author  Fernando Pérez-Sanz (\code{fernando.perez8@@um.es})
##' @author  Miriam Riquelme Pérez (\code{miriam.riquelmep@@gmail.com})
##' @seealso \code{\link{imputeSenators}}
##' @keywords classes
setClass("imputeSenator", slots = list(data = "data.frame", senators = "character", endCluster = "character"))
